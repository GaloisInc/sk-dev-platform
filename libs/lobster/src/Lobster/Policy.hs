{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  $Header$
Description :  Compiling Lobster policies to information flow graphs
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Compiles the Lobster high-level policy language to an information flow graph.
-}
module Lobster.Policy
  ( invertPosition
  , Context
  , nullContext
  , singletonContext
  , appendContext
  , consContext
  , stripContext
  , ContextClass(..)
  , mkContextClass
  , prettyPrintContextClass
  , Signature
  , memberClassSignature
  , Domain(..)
  , nameDomain
  , provenanceDomain
  , getSubDomain
  , getPortTypeDomain
  , typeCheckDomain
  , flattenDomain
  , foldSubDomain
  , foldMSubDomain
  , foldMConnectionsDomain
  , prettyPrintDomain
  , prettyPrintDomainPort
  , Value(..)
  , toIntValue
  , toClassValue
  , toPortTypeValue
  , Policy
  , empty
  , append
  , toDomain
  , interpretPolicy
  , parsePolicyFile
  , PortTypeValue(..)
  , PortType(..)
  , positionPortType
  , directionPortType
  , prettyPrintPortType
  )

where

import Control.Monad(foldM)
import Control.DeepSeq

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Lobster.Monad
import qualified Lobster.Abs as Abs
import Lobster.Abs(
  ClassId,
  Connection(..),
  Direction(..),
  Expression(..),
  FlowId,
  Identifier,
  Name(..),
  NoneExpression(..),
  PortId,
  Policy(..),
  PortTypeConstraint(..),
  Position(..),
  QualName(..),
  Statement(..),
  ClassInstantiation(..))
import qualified Lobster.Lex as Lex
import qualified Lobster.Par as Par
import qualified Lobster.ErrM as ErrM
import Lobster.Print (printTree)
import qualified Lobster.Syntax as Syntax
import qualified Lobster.Domain as Domain
import Lobster.Domain(
  DomainId,
  DomainPort,
  Assertion(..),
  Pred(..),
  PortRE(..))

--------------------------------------------------------------------------------
-- Helper functions.
--------------------------------------------------------------------------------

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

trim :: String -> String
trim = applyTwice (reverse . dropWhile Char.isSpace)

toInt :: Integer -> Int
toInt x
  | x > fromIntegral (maxBound :: Int) || x < fromIntegral (minBound :: Int) = error $ "ERROR:integer out of range:" ++ show x
  | otherwise = fromIntegral x

{-
mapMaybeP :: (a -> P b) -> Maybe a -> P (Maybe b)
mapMaybeP f xo =
    case xo of
      Nothing -> return Nothing
      Just x ->
          do y <- f x
             return (Just y)
-}

--------------------------------------------------------------------------------
-- Does a port act in the subject or object position for an allow statement?
--------------------------------------------------------------------------------

-- data Position =
--     SubjectPosition
--   | ObjectPosition
--   deriving (Eq, Read, Show, Ord)

unifyPosition :: Position -> Position -> Maybe Position
unifyPosition p1 p2 = if p1 == p2 then Just p1 else Nothing

invertPosition :: Position -> Position
invertPosition pos =
    case pos of
      SubjectPosition -> ObjectPosition
      ObjectPosition -> SubjectPosition

connectablePosition :: Position -> Position -> Bool
connectablePosition p1 p2 = p2 == invertPosition p1

prettyPrintPosition :: Position -> String
prettyPrintPosition position =
    case position of
      SubjectPosition -> "subject"
      ObjectPosition -> "object"

--------------------------------------------------------------------------------
-- Policy functions.
--------------------------------------------------------------------------------

empty :: Policy
empty = Policy []

append :: Policy -> Policy -> Policy
append (Policy s1) (Policy s2) = Policy (s1 ++ s2)

--------------------------------------------------------------------------------
-- Contexts are class nestings.
--------------------------------------------------------------------------------

newtype Context = Context [ClassId]
  deriving (Eq, Show, Ord)

instance NFData Context where
  rnf (Context a) = rnf a

emptyContext :: Context
emptyContext = Context []

nullContext :: Context -> Bool
nullContext (Context l) = null l

singletonContext :: ClassId -> Context
singletonContext cl = Context [cl]

appendContext :: Context -> Context -> Context
appendContext (Context c) (Context c') = Context (c ++ c')

consContext :: Context -> ClassId -> Context
consContext ctxt cl = appendContext ctxt (singletonContext cl)

stripContext :: Context -> Context -> Maybe Context
stripContext (Context c1) (Context c2) =
    case List.stripPrefix c1 c2 of
      Nothing -> Nothing
      Just c -> Just (Context c)

searchContext :: Context -> [Context]
searchContext =
    \ctxt ->
        let Context cls = ctxt in
        ctxt : mk (reverse cls)

  where
    mk rcls =
        case rcls of
          [] -> []
          _ : rcls' -> Context (reverse rcls') : mk rcls'

prettyPrintContext :: Context -> String
prettyPrintContext (Context l) =
    if null l then "<empty-context>"
    else List.intercalate "." (map Syntax.idString l)

--------------------------------------------------------------------------------
-- Classes in context.
--------------------------------------------------------------------------------

data ContextClass = ContextClass Context ClassId
  deriving (Eq, Show, Ord)

instance NFData ContextClass where
  rnf (ContextClass a b) = rnf a `seq` rnf b

mkContextClass :: ClassId -> ContextClass
mkContextClass = ContextClass emptyContext

unifyContextClass :: ContextClass -> ContextClass -> Maybe ContextClass
unifyContextClass t1 t2 = if t1 == t2 then Just t1 else Nothing

connectableContextClass :: ContextClass -> ContextClass -> Bool
connectableContextClass t1 t2 = t1 == t2

prettyPrintContextClass :: ContextClass -> String
prettyPrintContextClass (ContextClass c cl) =
    (if nullContext c then "" else prettyPrintContext c ++ ".") ++
    Syntax.idString cl

--------------------------------------------------------------------------------
-- Lobster class constructor functions.
--------------------------------------------------------------------------------

data ClassConstructor =
    ClassConstructor [Identifier] [Statement]
  deriving (Eq, Show, Ord)

nullClassConstructor :: ClassConstructor -> Bool
nullClassConstructor (ClassConstructor ids sts) = null ids && null sts

--------------------------------------------------------------------------------
-- Lobster signatures.
--------------------------------------------------------------------------------

data Signature =
    Signature
      {classes :: Map.Map ClassId (Signature,ClassConstructor)}
  deriving (Eq, Show, Ord)

emptySignature :: Signature
emptySignature =
    Signature
      {classes = Map.empty}

nullSignature :: Signature -> Bool
nullSignature sig = Map.null (classes sig)

lookupClassSignature ::
    Signature -> ClassId -> Maybe (Signature,ClassConstructor)
lookupClassSignature sig cl = Map.lookup cl (classes sig)

lookupContextSignature :: Signature -> Context -> Maybe Signature
lookupContextSignature =
    \sig ctxt ->
        let Context cls = ctxt in
        findcl cls sig
  where
    findcl cls sig =
        case cls of
          [] -> Just sig
          cl : cls' ->
              case lookupClassSignature sig cl of
                Nothing -> Nothing
                Just (sig',_) -> findcl cls' sig'

lookupContextClassSignature ::
    Signature -> ContextClass -> Maybe (Signature,ClassConstructor)
lookupContextClassSignature sig ccl =
    let ContextClass ctxt cl = ccl in
    case lookupContextSignature sig ctxt of
      Nothing -> Nothing
      Just sig' -> lookupClassSignature sig' cl

memberClassSignature :: Signature -> ClassId -> Bool
memberClassSignature sig cl =
    case lookupClassSignature sig cl of
      Just _ -> True
      Nothing -> False

addClassSignature ::
    Signature -> ClassId -> Signature -> ClassConstructor -> P Signature
addClassSignature sig cl insig clcon =
    if Maybe.isJust (lookupClassSignature sig cl)
      then throwError ("duplicate class declaration " ++ show cl)
      else return (sig {classes = Map.insert cl (insig,clcon) (classes sig)})

addStatementSignature :: Signature -> Statement -> P Signature
addStatementSignature sig statement =
    case statement of
      ClassDeclaration cl pl sts ->
          do insig <- addStatementsSignature emptySignature sts
             sig' <- addClassSignature sig cl insig (ClassConstructor pl sts)
             return sig'
      _ -> return sig

addStatementsSignature :: Signature -> [Statement] -> P Signature
addStatementsSignature = foldM addStatementSignature

mkSignature :: [Statement] -> P Signature
mkSignature = addStatementsSignature emptySignature

--------------------------------------------------------------------------------
-- Signatures in context.
--------------------------------------------------------------------------------

data ContextSignature =
    ContextSignature Signature Context
  deriving (Eq, Show, Ord)

mkContextSignature :: Signature -> ContextSignature
mkContextSignature sig = ContextSignature sig emptyContext

lookupContextClassContextSignature ::
    ContextSignature -> ContextClass ->
    Maybe (ContextSignature,Signature,ClassConstructor)
lookupContextClassContextSignature csig ccl =
    let ContextSignature sig _ = csig in
    case lookupContextClassSignature sig ccl of
      Nothing -> Nothing
      Just (clsig,clcon) ->
          let ContextClass ctxt cl = ccl in
          let ctxt' = consContext ctxt cl in
          Just (ContextSignature sig ctxt', clsig, clcon)

getContextClassContextSignature ::
    ContextSignature -> ContextClass ->
    P (ContextSignature,Signature,ClassConstructor)
getContextClassContextSignature sig cl =
    case lookupContextClassContextSignature sig cl of
      Nothing -> throwError $ "unknown class: " ++ show cl
      Just sig' -> return sig'

lookupClassContextSignature ::
    ContextSignature -> ClassId ->
    Maybe (ContextSignature,ContextClass,Signature,ClassConstructor)
lookupClassContextSignature csig cl =
    let ContextSignature _ ctxt = csig in
    let ctxts = searchContext ctxt in
    findcl ctxts

  where
    findcl ctxts =
        case ctxts of
          [] -> Nothing
          ctxt : ctxts' ->
              let ccl = ContextClass ctxt cl in
              case lookupContextClassContextSignature csig ccl of
                  Nothing -> findcl ctxts'
                  Just (csig',clsig,clcon) -> Just (csig',ccl,clsig,clcon)

getClassContextSignature ::
    ContextSignature -> ClassId ->
    P (ContextSignature,ContextClass,Signature,ClassConstructor)
getClassContextSignature sig cl =
    case lookupClassContextSignature sig cl of
      Nothing -> throwError $ "unknown class: " ++ show cl
      Just cl_sig_con -> return cl_sig_con

--------------------------------------------------------------------------------
-- Port type values.
--------------------------------------------------------------------------------

data PortTypeValue =
    TypePortTypeValue ContextClass
  | PositionPortTypeValue Position
  deriving (Eq, Show, Ord)

instance NFData PortTypeValue where
  rnf x = case x of
    TypePortTypeValue a -> rnf a
    PositionPortTypeValue a -> rnf a

unifyPortTypeValue :: PortTypeValue -> PortTypeValue -> Maybe PortTypeValue
unifyPortTypeValue v1 v2 =
    case (v1,v2) of
      (TypePortTypeValue t1, TypePortTypeValue t2) ->
          (case unifyContextClass t1 t2 of
             Just t -> Just (TypePortTypeValue t)
             Nothing -> Nothing)
      (PositionPortTypeValue p1, PositionPortTypeValue p2) ->
          (case unifyPosition p1 p2 of
             Just p -> Just (PositionPortTypeValue p)
             Nothing -> Nothing)
      _ -> Nothing

connectablePortTypeValue :: PortTypeValue -> PortTypeValue -> Bool
connectablePortTypeValue v1 v2 =
    case (v1,v2) of
      (TypePortTypeValue t1, TypePortTypeValue t2) ->
          connectableContextClass t1 t2
      (PositionPortTypeValue p1, PositionPortTypeValue p2) ->
          connectablePosition p1 p2
      _ -> False

prettyPrintPortTypeValue :: PortTypeValue -> String
prettyPrintPortTypeValue v =
    case v of
      TypePortTypeValue t -> prettyPrintContextClass t
      PositionPortTypeValue p -> prettyPrintPosition p

--------------------------------------------------------------------------------
-- Port types.
--------------------------------------------------------------------------------

newtype PortType = PortType (Domain.PortType PortTypeValue)
  deriving (Eq, Show, Ord)

instance NFData PortType where
  rnf (PortType a) = rnf a

anyPortType :: PortType
anyPortType = PortType Domain.anyPortType

singletonPortType :: FlowId -> Domain.PortTypeValue PortTypeValue -> PortType
singletonPortType f v = PortType (Domain.singletonPortType f v)

lookupPortType :: PortType -> FlowId -> Maybe (Domain.PortTypeValue PortTypeValue)
lookupPortType (PortType pt) f = Domain.lookupPortType pt f

unifyPortType :: PortType -> PortType -> PortType
unifyPortType (PortType pt1) (PortType pt2) =
    PortType (Domain.unifyPortType unifyPortTypeValue pt1 pt2)

directionPortType :: PortType -> Maybe Direction
directionPortType (PortType pt) = Domain.directionPortType pt

prettyPrintPortType :: PortType -> String
prettyPrintPortType (PortType pt) =
    Domain.prettyPrintPortType prettyPrintPortTypeValue pt

--------------------------------------------------------------------------------
-- Port positions.
--------------------------------------------------------------------------------

positionPortType :: PortType -> Maybe Position
positionPortType pt =
    case lookupPortType pt Syntax.positionFlow of
      Just (Domain.Value (PositionPortTypeValue p)) -> Just p
      _ -> Nothing

--------------------------------------------------------------------------------
-- Domain provenances.
--------------------------------------------------------------------------------

prettyPrintProvenance :: (ContextClass,[Value]) -> String
prettyPrintProvenance (ccl,v) =
    prettyPrintContextClass ccl ++
    ("(" ++ List.intercalate ", " (map prettyPrintValue v) ++ ")")

--------------------------------------------------------------------------------
-- Domains.
--------------------------------------------------------------------------------

newtype Domain = Domain (Domain.Domain (ContextClass,[Value]) PortTypeValue)

instance NFData Domain where
  rnf (Domain a) = rnf a

emptyDomain :: String -> (ContextClass,[Value]) -> Domain
emptyDomain n ccl = Domain (Domain.empty n ccl)

nameDomain :: Domain -> String
nameDomain (Domain d) = Domain.name d

provenanceDomain :: Domain -> (ContextClass,[Value])
provenanceDomain (Domain d) = Domain.value d

portsDomain :: Domain -> Map.Map PortId (Domain.PortType PortTypeValue)
portsDomain (Domain dom) = Domain.ports dom

getPortTypeDomain :: Domain -> DomainPort -> P PortType
getPortTypeDomain (Domain dom) dp =
    do pt <- Domain.getPortType dom dp
       return (PortType pt)

addPortDomain :: Domain -> PortId -> PortType -> P Domain
addPortDomain (Domain dom) pid (PortType pty) =
    do d <- Domain.addPort dom pid pty
       return (Domain d)

getSubDomain :: Domain -> DomainId -> P Domain
getSubDomain (Domain dom) did =
    do s <- Domain.getSubDomain dom did
       return (Domain s)

addSubDomain :: Domain -> Domain -> (Domain,DomainId)
addSubDomain (Domain dom) (Domain sub) =
    let (dom',did) = Domain.addSubDomain dom sub in
    (Domain dom', did)

foldSubDomain :: (DomainId -> Domain -> s -> s) -> s -> Domain -> s
foldSubDomain f =
    \x (Domain d) -> Domain.foldSubDomain f' x d
    where
      f' i d x = f i (Domain d) x

foldMSubDomain :: (DomainId -> Domain -> s -> P s) -> s -> Domain -> P s
foldMSubDomain f =
    \x (Domain d) -> Domain.foldMSubDomain f' x d
    where
      f' i d x = f i (Domain d) x

addConnectionsDomain ::
    Domain -> DomainPort -> Connection -> [DomainPort] -> P Domain
addConnectionsDomain (Domain dom) dp conn dps =
    do dom' <- Domain.addConnections dom dp conn dps
       return (Domain dom')

addPortConnectionsDomain ::
    Domain -> [DomainPort] -> Connection -> [DomainPort] -> P Domain
addPortConnectionsDomain (Domain dom) dps1 conn dps2 =
    do dom' <- Domain.addPortConnections dom dps1 conn dps2
       return (Domain dom')

foldMConnectionsDomain ::
    (DomainPort -> Connection -> DomainPort -> s -> P s) -> s -> Domain -> P s
foldMConnectionsDomain f x (Domain d) = Domain.foldMConnections f x d

typeCheckDomain :: Domain -> P ()
typeCheckDomain (Domain dom) =
    Domain.typeCheck unifyPortTypeValue connectablePortTypeValue
      prettyPrintPortTypeValue dom

flattenDomain :: Domain -> P Domain
flattenDomain (Domain dom) =
    do dom' <- Domain.flatten dom
       return (Domain dom')

prettyPrintDomain :: Domain -> String
prettyPrintDomain (Domain dom) =
    Domain.prettyPrint prettyPrintProvenance prettyPrintPortTypeValue dom

addAssertion :: Domain -> Assertion -> Domain
addAssertion (Domain d) a = Domain $ Domain.addAssertion d a

checkAssertionsDomain :: Domain -> [Either String String]
checkAssertionsDomain (Domain d) = Domain.checkAssertions_ d

--------------------------------------------------------------------------------
-- Domain ports.
--------------------------------------------------------------------------------

prettyPrintDomainPort :: Domain -> DomainPort -> String
prettyPrintDomainPort (Domain dom) dp = Domain.prettyPrintDomainPort dom dp

--------------------------------------------------------------------------------
-- The system domain.
--------------------------------------------------------------------------------

systemName :: String
systemName = ""

systemContextClass :: ContextClass
systemContextClass = ContextClass emptyContext Syntax.systemClass

systemProvenance :: (ContextClass,[Value])
systemProvenance = (systemContextClass,[])

systemDomain :: Domain
systemDomain = emptyDomain systemName systemProvenance

--------------------------------------------------------------------------------
-- Policy values.
--------------------------------------------------------------------------------

data Value =
    IntValue Int
  | StringValue String
  | DirectionValue Direction
  | PositionValue Position
  | ClassValue ContextClass
  | PortTypeValue PortType
  | DomainValue Domain.DomainId
  | DomainPortValue Domain.DomainPort
  deriving (Eq, Show, Ord)

instance NFData Value where
  rnf x = case x of
    IntValue a -> rnf a
    StringValue a -> rnf a
    DirectionValue a -> rnf a
    PositionValue a -> rnf a
    ClassValue a -> rnf a
    PortTypeValue a -> rnf a
    DomainValue a -> rnf a
    DomainPortValue a -> rnf a

{-
toStringValue :: String -> Value -> P String
toStringValue err val =
    case val of
      StringValue s -> return s
      _ -> throwError (err ++ ": " ++ show val)
-}

toIntValue :: String -> Value -> P Int
toIntValue err val =
    case val of
      IntValue i -> return i
      _ -> throwError (err ++ ": " ++ show val)

toClassValue :: String -> Value -> P ContextClass
toClassValue err val =
    case val of
      ClassValue ty -> return ty
      _ -> throwError (err ++ ": " ++ show val)

toPortTypeValue :: String -> Value -> P PortType
toPortTypeValue err val =
    case val of
      PortTypeValue pty -> return pty
      _ -> throwError (err ++ ": " ++ show val)

toDomainPortValue :: String -> Value -> P Domain.DomainPort
toDomainPortValue err val =
    case val of
      DomainPortValue p -> return p
      _ -> throwError (err ++ ": " ++ show val)

toDomainPortsValue :: String -> [Value] -> P [Domain.DomainPort]
toDomainPortsValue err = mapM (toDomainPortValue err)

toPortTypeValueValue ::
    ContextSignature -> Value -> P (Domain.PortTypeValue PortTypeValue)
toPortTypeValueValue sig v =
    case v of
      DirectionValue d -> return (Domain.Direction d)
      PositionValue p -> return (Domain.Value (PositionPortTypeValue p))
      ClassValue ccl ->
          case lookupContextClassContextSignature sig ccl of
            Nothing -> throwError ("no such class: " ++ show ccl)
            Just (_,clsig,clcon) ->
                if nullSignature clsig && nullClassConstructor clcon
                  then return (Domain.Value (TypePortTypeValue ccl))
                  else throwError ("non-empty class: " ++ show ccl)
      _ -> throwError ("bad port type value: " ++ show v)

prettyPrintValue :: Value -> String
prettyPrintValue v =
    case v of
      IntValue i -> show i
      StringValue s -> show s
      DirectionValue d -> Domain.prettyPrintDirection d
      PositionValue p -> prettyPrintPosition p
      ClassValue ccl -> prettyPrintContextClass ccl
      PortTypeValue p -> prettyPrintPortType p
      DomainValue d -> show d
      DomainPortValue p -> show p

--------------------------------------------------------------------------------
-- Symbion assertions.
--------------------------------------------------------------------------------

fromConnRE :: ContextSignature -> Environment -> Domain -> Abs.ConnRE -> P Domain.PortRE
fromConnRE sig env obj conn@(Abs.ConnRE spec re) = case (spec,re) of
  (Abs.ThisDom, Abs.AnyPRE) -> return $ Domain.ThisAnyPortRE
  (Abs.IdentDom a, Abs.AnyPRE) -> do
    v <- evaluateNameExpression sig env (Ident a)
    case v of
      DomainValue i -> return $ Domain.AnyPortRE i
      _ -> throwError $ "expected domain identifier:" ++ show a
  (Abs.ThisDom, Abs.IdPRE b) -> evaluateNameExpression sig env (Ident b) >>= toPortDPRE
  (Abs.IdentDom a, Abs.IdPRE b) -> do
    v <- evaluateQualNameExpression sig env obj (Qual (UnQual (Ident a)) (Ident b))
    toPortDPRE v
  where
  toPortDPRE v = case v of
    DomainPortValue p -> return $ Domain.PortRE p
    _ -> throwError $ "expected port identifier:" ++ show conn

fromFlowPred ::
  ContextSignature -> Environment -> Domain -> PortRE -> PortRE -> Abs.FlowPred -> P Pred
fromFlowPred sig env obj a b x = do
  prd <- case x of
    Abs.NeverPathFP -> return $ NoPathPred a b
    Abs.ExistsPathFP -> return $ IsPathPred a b
    Abs.PathFP y -> do
      p <- fromFlowRE sig env obj a b y
      return $ OrPred (NoPathPred a b) p
  return prd

fromFlowRE ::
  ContextSignature -> Environment -> Domain -> PortRE -> PortRE -> Abs.FlowRE -> P Pred
fromFlowRE sig env obj a b x = do
  prd <- case x of
    Abs.AnyFRE -> return $ IsPathPred a b
    Abs.ConsF flowA conn flowB -> do
      c <- fromConnRE sig env obj conn
      p <- fromFlowRE sig env obj a c flowA
      let q = ViaPred a b c
      r <- fromFlowRE sig env obj c b flowB
      return $ AndPred p (AndPred q r)
  return prd

--------------------------------------------------------------------------------
-- Policy environments.
--------------------------------------------------------------------------------

newtype Environment = Environment (Map.Map Identifier Value)
  deriving (Eq, Show, Ord)

emptyEnvironment :: Environment
emptyEnvironment = Environment Map.empty

lookupEnvironment :: Environment -> Identifier -> Maybe Value
lookupEnvironment (Environment bindings) i = Map.lookup i bindings

addEnvironment :: Environment -> Identifier -> Value -> P Environment
addEnvironment env i val =
    case lookupEnvironment env i of
      Nothing ->
          case env of
            Environment bindings ->
                return (Environment (Map.insert i val bindings))
      Just _ -> throwError ("duplicate binding " ++ show i)

addListEnvironment :: Environment -> [(Identifier,Value)] -> P Environment
addListEnvironment =
    foldM add
    where
      add env (i,val) = addEnvironment env i val

mkEnvironment :: [(Identifier,Value)] -> P Environment
mkEnvironment = addListEnvironment emptyEnvironment

fromSubDomainEnvironment :: Domain -> Domain.DomainId -> P Environment
fromSubDomainEnvironment obj oid =
    do o <- getSubDomain obj oid
       foldM add emptyEnvironment (Map.keys (portsDomain o))
    where
      add env p =
          let v = DomainPortValue (Domain.internalDomainPort oid p) in
          addEnvironment env (Syntax.toId p) v

evaluatePortTypeConstraints ::
    ContextSignature -> Environment -> Domain -> [PortTypeConstraint] ->
    P PortType
evaluatePortTypeConstraints sig env dom =
    foldM eval anyPortType
    where
      eval pt (PortTypeConstraint f ne) =
          do v <- case ne of
                    NoneE -> return Domain.Inconsistent
                    SomeE e -> do p <- evaluateExpression sig env dom e
                                  toPortTypeValueValue sig p
             return (unifyPortType pt (singletonPortType f v))

evaluateExpression ::
    ContextSignature -> Environment -> Domain -> Expression ->
    P Value
evaluateExpression sig env obj expr =
    case expr of
      IntExpression i -> return (IntValue $ toInt i)
      StringExpression s -> return (StringValue s)
      DirectionExpression d -> return (DirectionValue d)
      PositionExpression p -> return (PositionValue p)
      QualNameExpression qn -> evaluateQualNameExpression sig env obj qn
      ParenExpression expr1 -> evaluateExpression sig env obj expr1
    `catchError`
    (\e -> throwError $ "in expression " ++ show expr ++ ":\n" ++ e)

evaluateQualNameExpression ::
    ContextSignature -> Environment -> Domain -> QualName -> P Value
evaluateQualNameExpression sig env obj qn = case qn of
  UnQual n -> evaluateNameExpression sig env n
  Qual n1 n2 ->
          do let e2 = QualNameExpression $ UnQual n2
             v1 <- evaluateQualNameExpression sig env obj n1
             case v1 of
               ClassValue c1 ->
                   do (sig',_,_) <- getContextClassContextSignature sig c1
                      v2 <- evaluateExpression sig' emptyEnvironment obj e2
                      case v2 of
                        ClassValue _ -> return v2
                        _ -> throwError $ "bad value in a class context: (" ++
                                          show v1 ++ ") . (" ++ show v2 ++ ")"
               DomainValue o1 ->
                   do env' <- fromSubDomainEnvironment obj o1
                      v2 <- evaluateExpression sig env' obj e2
                      case v2 of
                        DomainPortValue _ -> return v2
                        _ -> throwError $ "bad value in an domain context: (" ++
                                          show v1 ++ ") . (" ++ show v2 ++ ")"
               _ -> throwError $ "bad context value: (" ++
                                 show v1 ++ ") . _"

evaluateNameExpression :: ContextSignature -> Environment -> Name -> P Value
evaluateNameExpression sig env n = case n of
      Ident i ->
          case lookupEnvironment env i of
            Just val -> return val
            Nothing -> throwError $ "no such identifier " ++ show i
      TypeIdent i ->
          case lookupClassContextSignature sig (Syntax.mkId $ Syntax.idString i) of
            Just (_,ccl,_,_) -> return (ClassValue ccl)
            Nothing -> throwError $ "no such type identifier " ++ show i

evaluateExpressions ::
    ContextSignature -> Environment -> Domain -> [Expression] -> P [Value]
evaluateExpressions sig env obj exprs =
    case exprs of
      [] -> return []
      expr : exprs' ->
          do v <- evaluateExpression sig env obj expr
             vs <- evaluateExpressions sig env obj exprs'
             return (v : vs)

interpretPolicy :: Policy -> ([Either String String],Domain)
interpretPolicy policy =
    case runP (toDomain policy) of
      Left err ->
          error ("ERROR: couldn't interpret the Lobster policy file:\n" ++ err)
      Right x -> x

parsePolicyFile :: FilePath -> IO Policy
parsePolicyFile filename =
    do chars <- readFile filename
       let toks = Lex.tokens chars
       case Par.pPolicy toks of
         ErrM.Bad e -> error $ "ERROR: unable to parse\n:" ++ e
         ErrM.Ok policy -> return policy

interpretStatement ::
    ContextSignature -> Environment -> Domain -> Statement ->
    P (Environment,Domain)
interpretStatement sig env obj statement =
    case statement of
      Assert connA connB flowPred -> do
        a <- fromConnRE sig env obj connA
        b <- fromConnRE sig env obj connB
        c <- fromFlowPred sig env obj a b flowPred
        let ca = Assertion (trim (printTree statement)) c
        return (env, addAssertion obj ca)
      DomainDeclaration n (ClassInstantiation cl el) ->
          do (clsig, ccl, _, ClassConstructor ids sts) <-
                 getClassContextSignature sig cl
             args' <- evaluateExpressions sig env obj el
             clenv <- mkEnvironment (zip ids args')
             let clobj = emptyDomain (Syntax.idString n) (ccl,args')
             (_,clobj') <- interpretStatements clsig clenv clobj sts
             let (obj',clobjid) = addSubDomain obj clobj'
             let val = DomainValue clobjid
             env' <- addEnvironment env n val
             return (env',obj')
      ClassDeclaration _ _ _ -> return (env,obj)
      PortDeclaration pid pty pconns ->
          let ptce = Syntax.toConstraintsPortDeclarationType pty in
          do ptc <- evaluatePortTypeConstraints sig env obj ptce
             obj' <- addPortDomain obj pid ptc
             let p = Domain.externalDomainPort pid
             obj'' <-
                 case pconns of
                   Abs.EmptyPDC -> return obj'
                   Abs.Connection conn ps ->
                       do ps' <- evaluateExpressions sig env obj' ps
                          ps'' <- toDomainPortsValue "bad domain port" ps'
                          addConnectionsDomain obj' p conn ps''
             env' <- addEnvironment env (Syntax.toId pid) (DomainPortValue p)
             return (env',obj'')
      PortConnection ps1 conn ps2 ->
          do ps1' <- evaluateExpressions sig env obj ps1
             ps1'' <- toDomainPortsValue "bad domain port" ps1'
             ps2' <- evaluateExpressions sig env obj ps2
             ps2'' <- toDomainPortsValue "bad domain port" ps2'
             obj' <- addPortConnectionsDomain obj ps1'' conn ps2''
             return (env,obj')
      Assignment i expr ->
          do val <- evaluateExpression sig env obj expr
             env' <- addEnvironment env i val
             return (env',obj)
    `catchError`
    (\e -> throwError $ "in statement " ++ trim (printTree statement) ++
           "\n" ++ e)

interpretStatements ::
    ContextSignature -> Environment -> Domain -> [Statement] ->
    P (Environment,Domain)
interpretStatements sig env obj sts =
    case sts of
      [] -> return (env,obj)
      st:sts' ->
          do (env',obj') <- interpretStatement sig env obj st
             interpretStatements sig env' obj' sts'

toDomain :: Policy -> P ([Either String String],Domain)
toDomain (Policy sts) =
    do sig <- mkSignature sts
       let csig = mkContextSignature sig
       (_,obj) <- interpretStatements csig emptyEnvironment systemDomain sts
       typeCheckDomain obj
       let eeas = checkAssertionsDomain obj
       return (eeas,obj)
