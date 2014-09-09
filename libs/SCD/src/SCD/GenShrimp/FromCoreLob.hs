{-# OPTIONS -Wall #-}

module FromCoreLob (fromPolicy,ppPolicyTE,ppPolicyFC) where

-- FromCoreLob transforms core lobster into core shrimp (i.e. SELinux M4)

-- LIMITATIONS:
-- Doesn't support top-level ports.
-- Doesn't support primitive ports without position constraint.
-- Doesn't generate version statements.
-- Doesn't generate file statements.

import AbsCoreLob (Version(..))
import Control.Monad.State
import Data.Char
import Data.Map hiding (map,null,lookup)
import Data.Maybe
import Text.PrettyPrint
import qualified AbsCoreLob as C
import qualified Text.PrettyPrint as PP

type M a = State Policy a

data Decls = Decls
  { classDecls :: [Class]
  , assignDecls :: [(VarName,Expr)]
  , domainDecls :: [((VarName,ClassName),[Expr])]
  , portDecls :: Map PortName Port
  , adapterCalls :: [(Ifc,Ifc)]
  }
  deriving (Show)

data Class = Class
  { className :: ClassName
  , classArgs :: [VarName]
  , localDecls :: Decls
  }
  deriving (Show)

data Port = Port
  { portPosition :: C.PositionValue
  , portIfcs :: [Ifc]
  }
  deriving (Show)

data Policy = Policy
  { policyName :: PolicyName
  , version :: Version
  , decls :: Decls
  } deriving (Show)

data Expr
  = EIdent VarName
  | ENum Integer
  | EStr String
  deriving (Show)

type Ifc = (VarName,PortName)

type Env = [(VarName,ClassName)]

type PolicyName = String
type ClassName = String
type PortName = String
type VarName = String

fromPolicy :: C.Policy -> Policy
fromPolicy (C.Policy n v ss) =
  execState (convertStmts ss) $ Policy (fromIdentifier n) v zeroDecls

zeroDecls :: Decls
zeroDecls = Decls [] [] [] Data.Map.empty []

withClass :: ClassName -> [VarName] -> M () -> M ()
withClass n vs m = do
  st <- get
  let ds = decls st
  put $ st{ decls = zeroDecls }
  m
  st1 <- get
  put $ st{ decls = ds{ classDecls = classDecls ds ++ [Class n vs $ decls st1] } }

convertStmts :: [C.Stmt] -> M ()
convertStmts ss = mapM_ convertStmt ss

modifyDeclsM :: (Decls -> Decls) -> M ()
modifyDeclsM f = modifyM $ \st -> st{ decls = f (decls st) }

modifyM :: (Policy -> Policy) -> M ()
modifyM f = do
  st <- get
  put $ f st

addPortDecl :: C.PositionValue -> PortName -> M ()
addPortDecl pv n = do
  modifyDeclsM $ \st -> st{ portDecls = insert n (Port pv []) $ portDecls st }

addDomainDecl :: VarName -> ClassName -> [Expr] -> M ()
addDomainDecl i t es = modifyDeclsM $ \st -> st{ domainDecls = domainDecls st ++ [((i,t),es)] }

addCoupler ::  PortName -> Ifc -> M ()
addCoupler n ifc = modifyPort n $ \p -> Just $ p{ portIfcs = portIfcs p ++ [ifc] }

modifyPort :: PortName -> (Port -> Maybe Port) -> M ()
modifyPort n f = modifyDeclsM $ \st -> st{ portDecls = update f n $ portDecls st }

addAssignDecl :: VarName -> Expr -> M ()
addAssignDecl i e = modifyDeclsM $ \st -> st{ assignDecls = assignDecls st ++ [(i,e)] }

addAdapter :: Ifc -> Ifc -> M ()
addAdapter ifc1 ifc2 =
  modifyDeclsM $ \st -> st{ adapterCalls = adapterCalls st ++ [(ifc1,ifc2)] }

fromIdentifier :: C.Identifier -> String
fromIdentifier (C.Identifier i) = i

fromTypeIdentifier :: C.TypeIdentifier -> String
fromTypeIdentifier (C.TypeIdentifier t) = t

convertStmt :: C.Stmt -> M ()
convertStmt a = case a of
  C.Class t vs ss -> withClass (fromTypeIdentifier t) (map fromIdentifier vs) $ convertStmts ss
  C.Port pv i -> addPortDecl pv $ fromIdentifier i
  C.Assign i e -> addAssignDecl (fromIdentifier i) (fromExpr e)
  C.Domain i t es -> addDomainDecl (fromIdentifier i) (fromTypeIdentifier t) (map fromExpr es)
  C.Adapter qi1 qi2 ->
    addAdapter (qualIdentToIfc qi1) (qualIdentToIfc qi2)
  C.Coupler i qi ->
    addCoupler (fromIdentifier i) (qualIdentToIfc qi)

qualIdentToIfc :: C.QualIdent -> Ifc
qualIdentToIfc (C.Qual i1 i2) = (fromIdentifier i1, fromIdentifier i2)

fromExpr :: C.Expr -> Expr
fromExpr e = case e of
  C.Num i -> ENum i
  C.Str s -> EStr s

ppPolicyFC :: Policy -> Doc
ppPolicyFC p = vcat
  [ ppLocalDeclsFC $ decls p
  , ppCallsFC Nothing $ decls p
  ]

ppPolicyTE :: Policy -> Doc
ppPolicyTE p = vcat
  [ ppPolicyModule p
  , text "include(prelude.m4)"
  , ppLocalDecls Nothing $ decls p
  , ppCalls Nothing $ decls p
  ]

ppPolicyModule :: Policy -> Doc
ppPolicyModule p = hcat
  [ text "policy_module("
  , text $ policyName p
  , text ","
  , ppVersion $ version p
  , text ")" 
  ]

ppVersion :: Version -> Doc
ppVersion (Version x y) = text (show x) <> text "." <> text (show y)

ppLocalDeclsFC :: Decls -> Doc
ppLocalDeclsFC d = vcat $
  map ppClassDeclFC (classDecls d)

ppLocalDecls :: Maybe ClassName -> Decls -> Doc
ppLocalDecls mn d = vcat $
  map ppClassDecl (classDecls d) ++
  map (ppPortDecl env mn) (toList $ portDecls d)
  where
  env = getEnv d

ppCalls :: Maybe ClassName -> Decls -> Doc
ppCalls mn d = vcat $
  [ ppPushDef v e | (v,e) <- xs ] ++
  [ ppDomainDecl (isNothing mn) c v es | ((v,c),es) <- domainDecls d ] ++
  map (ppAdapterCall env $ isNothing mn) (adapterCalls d) ++
  [ ppPopDef v | (v,_) <- xs ]
  where
  env = getEnv d
  xs = assignDecls d

ppCallsFC :: Maybe ClassName -> Decls -> Doc
ppCallsFC mn d = vcat $
  [ ppPushDef v e | (v,e) <- xs ] ++
  [ ppDomainDeclFC (isNothing mn) c v es | ((v,c),es) <- domainDecls d ] ++
  [ ppPopDef v | (v,_) <- xs ]
  where
  xs = assignDecls d

getEnv :: Decls -> Env
getEnv d = map fst $ domainDecls d

ppPortDecl :: Env -> Maybe String -> (PortName, Port) -> Doc
ppPortDecl _ Nothing _ = error "unsupported:top level port declaration"
ppPortDecl env (Just c) (n,p) = case portIfcs p of
  [] -> case portPosition p of
    C.Subject -> ppPrimSubjIfc c n
    C.Object -> ppPrimObjIfc c n
    C.Unknown -> error $ "primitive ports without position declarations are unsupported:" ++ show n
  ifcs -> ppCoupler env c n ifcs

ppClassDecl :: Class -> Doc
ppClassDecl c = vcat $
  [ ppLocalDecls (Just n) d
  , text $ "define(Domain" ++ n ++ ",`"
  , nest 2 $ vcat $
    [ ppPushDef v (EIdent $ "$" ++ show i) | (v,i) <- zip vs [2 :: Int ..] ] ++
    [ if isPrimDecl then text "type $1_t;" else PP.empty
    , ppCalls (Just n) d
    ] ++
    [ ppPopDef v | v <- vs]
  , text "')"
  ]
  where
  n = className c
  d = localDecls c
  vs = classArgs c
  isPrimDecl = any null $ map portIfcs $ elems $ portDecls $ localDecls c
--  isPrimDecl = null (domainDecls d) && null (classDecls d) && null (adapterCalls d)

ppClassDeclFC :: Class -> Doc
ppClassDeclFC c = vcat $
  [ ppLocalDeclsFC d
  , text $ "define(DomainFC" ++ n ++ ",`"
  , nest 2 $ vcat $
    [ ppPushDef v (EIdent $ "$" ++ show i) | (v,i) <- zip vs [2 :: Int ..] ] ++
    [ ppBody
    ] ++
    [ ppPopDef v | v <- vs]
  , text "')"
  ]
  where
  n = className c
  d = localDecls c
  vs = classArgs c
  ppBody = case (n,vs) of
    ("File",[v]) -> text "$1_t " <> text v
    ("File",_) -> error "expecting exactly one argument in File class definition"
    _ -> ppCallsFC (Just n) d

ppPushDef :: VarName -> Expr -> Doc
ppPushDef v e = hcat [ text $ "pushdef(`" ++ v ++ "', `", ppExpr e, text "')" ]

ppPopDef :: VarName -> Doc
ppPopDef v = text $ "popdef(`" ++ v ++ "')"

ppCoupler :: Env -> ClassName -> PortName -> [Ifc] -> Doc
ppCoupler env c0 p0 vps = vcat
  [ text $ "define(Ifc" ++ c0 ++ "_" ++ p0 ++ ",`"
  , nest 2 $ sepWith (\d1 d2 -> vcat [d1 <> text ",", d2])
      [ ppCouplerCall env False p v | (v,p) <- vps ] <> text "')"
  ]

ppPrimSubjIfc :: ClassName -> PortName -> Doc
ppPrimSubjIfc c p = text $ "define(Ifc" ++ c ++ "_" ++ p ++ ",`$1_t')"

ppPrimObjIfc :: ClassName -> PortName -> Doc
ppPrimObjIfc c p = text $
  "define(Ifc" ++ c ++ "_" ++ p ++ ",`$1_t:" ++ map toLower c ++ " " ++ p ++ "')"

localPrefix :: Bool -> VarName -> Doc
localPrefix isTop v = if isTop then text v else (text $ "$1_" ++ v)

ppDomainDecl :: Bool -> ClassName -> VarName -> [Expr] -> Doc
ppDomainDecl isTop c v es = hcat
  [ text $ "Domain" ++ c ++ "("
  , sepWith (\d1 d2 -> d1 <> text ", " <> d2) (localPrefix isTop v : map ppExpr es)
  , text ")"
  ]

ppDomainDeclFC :: Bool -> ClassName -> VarName -> [Expr] -> Doc
ppDomainDeclFC isTop c v es = hcat
  [ text $ "DomainFC" ++ c ++ "("
  , sepWith (\d1 d2 -> d1 <> text ", " <> d2) (localPrefix isTop v : map ppExpr es)
  , text ")"
  ]

sepWith :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
sepWith _ [] = PP.empty
sepWith _ [s] = s
sepWith f (s:ss) = f s (sepWith f ss)

ppExpr :: Expr -> Doc
ppExpr e = text $ case e of
  EIdent s -> s
  ENum i -> show i
  EStr s -> show s

ppCouplerCall :: Env -> Bool -> PortName -> VarName -> Doc
ppCouplerCall env isTop p v = hcat
  [ text $ "Ifc" ++ c ++ "_" ++ p ++ "("
  , localPrefix isTop v
  , text ")"
  ]
  where
  c = lookupEnv env v

ppIfcCall :: Env -> Bool -> PortName -> VarName -> Doc
ppIfcCall env isTop p v = hcat
  [ text $ "Ifc" ++ c ++ "_" ++ p ++ "("
  , localPrefix isTop v
  , text ")"
  ]
  where
  c = lookupEnv env v

ppAdapterCall :: Env -> Bool -> (Ifc,Ifc) -> Doc
ppAdapterCall env isTop ((v1,p1),(v2,p2)) = hcat
  [ text "Connect(`"
  , ppIfcCall env isTop p1 v1
  , text "',`"
  , ppIfcCall env isTop p2 v2
  , text "')"
  ]

lookupEnv :: Env -> VarName -> ClassName
lookupEnv env v = case lookup v env of
  Nothing -> error $ "unknown variable:" ++ v
  Just c -> c

