{-# OPTIONS -Wall #-}
module Main (main) where

import AbsSimpleLob
import PrintSimpleLob
import QC

import Data.Map (Map)
import qualified Data.Map as M
import System
import Data.Maybe
import Control.Monad.State

type Gen a = GenT St a

main :: IO ()
main = do
  args <- getArgs
  let
    n = case args of
      [] -> 10
      (nStr:_) -> read nStr
  asts <- evalGenT zeroSt $ do
    genListLen n genPolicy
  mapM_ writeLSRFile $ zip asts [ 1 :: Int .. ]
  return ()

writeLSRFile :: (Policy,Int) -> IO ()
writeLSRFile (ast,i) = do
  putStrLn s
  writeFile fn s
  where
  fn = "TestPolicy" ++ pad 3 '0' i ++ ".lsr"
  s = unlines [ "# policy:" ++ fn, printTree ast ]

pad :: Show a => Int -> Char -> a -> String
pad i c x = replicate (i - length s) c ++ s
  where
  s = show x

data St = St
  { nextIdent :: Map String Int
  , externalPorts :: [(Name,PositionValue)]
  , localPorts :: [(QualName,PositionValue)]
  , classes :: [(TypeIdentifier, [(Name,PositionValue)])]
  }

getPorts :: Gen [(QualName,PositionValue)]
getPorts = do
  st <- getSt
  return $ localPorts st ++ [ (UnQual n, v) | (n,v) <- externalPorts st ]

zeroSt :: St
zeroSt = St M.empty [] [] []

genPortIdentifier :: Gen Identifier
genPortIdentifier = liftM Identifier $ genIdent "p"

genNumIdentifier :: Gen Identifier
genNumIdentifier = liftM Identifier $ genIdent "i"

genStringIdentifier :: Gen Identifier
genStringIdentifier = liftM Identifier $ genIdent "s"

genClassIdentifier :: Gen Identifier
genClassIdentifier = liftM Identifier $ genIdent "c"

genClassTypeIdentifier :: Gen TypeIdentifier
genClassTypeIdentifier = liftM TypeIdentifier $ genIdent "C"

genIdent :: String -> Gen String
genIdent s = do
  tbl <- liftM nextIdent getSt
  let i = maybe 0 succ $ M.lookup s tbl
  modifySt $ \st -> st{ nextIdent = M.insert s i tbl }
  return $ s ++ show i

genStmts :: Bool -> Gen [Stmt]
genStmts isTop = do
  modifyQCSize succ
  sz <- getQCSize
  let
    x = 50
    y = sz^sz
    z = if y <= 0 then 0 else x `div` y
  ss <- genListMaxLen z $ genStmt isTop
  modifyQCSize pred
  return ss

genPolicy :: Gen Policy
genPolicy = do
  setSt zeroSt
  setQCSize 0
  liftM (Policy []) $ genStmts True

isValidConnection :: Stmt -> Bool
isValidConnection (Connection subjs _ objs) = not (null subjs || null objs)
isValidConnection _ = False

genStmt :: Bool -> Gen Stmt
genStmt isTop = do
  let gs0 = [ genClassStmt, genAssignStmt ] -- fixme:implement type statement
  let gs1 = if isTop then gs0 else (genPortStmt : gs0)
  connStmt <- genConnectionStmt
  let gs2 = if isValidConnection connStmt then (return connStmt : gs1) else gs1
  cs <- liftM classes getSt
  let gs3 = if null cs then gs2 else (genDomainDecl cs : gs2)
  oneof gs3

genConnectionStmt :: Gen Stmt
genConnectionStmt = do
  s1 <- genPositionConnection Subject
  s2 <- genPositionConnection Object
  case (isValidConnection s1, isValidConnection s2) of
    (_,False) -> return s1
    (False,_) -> return s2
    _ -> elements [s1,s2]

genPositionConnection :: PositionValue -> Gen Stmt
genPositionConnection pv0 = do
  ps <- getPorts
  let l0 = map fst $ filter (\(n,pv) -> (isExternalPort n && pv == pv0) || (isLocalPort n && pv /= pv0)) ps
  let r0 = map fst $ filter (\(n,pv) -> isLocalPort n && pv == pv0) ps
  (l1,_) <- liftM (fromMaybe ([],[])) $ pickMinMaxN 1 (length l0) l0
  (r1,_) <- liftM (fromMaybe ([],[])) $ pickMinMaxN 1 (length r0) r0
  conn <- genConnection
  isSwapped <- genBool
  if isSwapped
    then return $ Connection r1 conn l1
    else return $ Connection l1 conn r1

isLocalPort :: QualName -> Bool
isLocalPort = not . isExternalPort

isExternalPort :: QualName -> Bool
isExternalPort (UnQual _) = True
isExternalPort _ = False

genClassStmt :: Gen Stmt
genClassStmt = do
  c <- genClassTypeIdentifier
  pl <- genParameterList
  st0 <- getSt
  setSt $ st0{ externalPorts = [], localPorts = []}
  ss <- genStmts False
  st1 <- getSt
  setSt $ st0{ classes = classes st0 ++ [(c, externalPorts st1)], nextIdent = nextIdent st1 }
  return $ Class c pl ss

genAssignStmt :: Gen Stmt
genAssignStmt = do
  e <- genExpr
  x <- case e of
    QualNameE _ -> error "unimplemented"
    Num _ -> genNumIdentifier
    Str _ -> genStringIdentifier
  return $ Assign x e

genPortStmt :: Gen Stmt
genPortStmt = do
  n <- genPortIdentifier
  pdt <- genMPortDeclarationType
  pdc <- genMPortDeclarationConnection
  modifySt $
    \st -> st{ externalPorts = externalPorts st ++ [(Ident n, positionValuePDT pdt)] }
  return $ Port n pdt pdc

positionValuePDT :: MPortDeclarationType -> PositionValue
positionValuePDT pdt = case pdt of
  EmptyPDT -> PolyPV
  FullPDT ptc -> positionValuePTC ptc

positionValuePTC :: [PortTypeConstraint] -> PositionValue
positionValuePTC ptcs = case [ pv | Position pv <- ptcs ] of
  [] -> PolyPV
  (pv:_) -> pv

genConnection :: Gen Connection
genConnection = elements
  [ Neutral
--   , BidirectionalC -- fixme: put back in
--   , LeftToRight -- fixme: put back in
--   , RightToLeft -- fixme: put back in
  ]

genMPortDeclarationType :: Gen MPortDeclarationType
genMPortDeclarationType = oneof
  [ liftM FullPDT genPortTypeConstraints
--   , return EmptyPDT
  ]

genMPortDeclarationConnection :: Gen MPortDeclarationConnection
genMPortDeclarationConnection = oneof
  [ return EmptyPDC
--  , liftM2 FullPDC genConnection genQualNames fixme: implement
  ]

genParameterList :: Gen [Identifier]
genParameterList = oneof
  [ return [] -- genIdentifiers fixme: implement
  ]

genPortTypeConstraints :: Gen [PortTypeConstraint]
genPortTypeConstraints = genListLen 1 genPortTypeConstraint

genPortTypeConstraint :: Gen PortTypeConstraint
genPortTypeConstraint = oneof
  [ liftM Position genPositionValue
--   , liftM Input genIntegerValue
--   , liftM Output genIntegerValue
--  , liftM TypePTC genTypeValue
--   , liftM Direction genDirectionValue
  ]

genPositionValue :: Gen PositionValue
genPositionValue = oneof
  [ return Subject
  , return Object
--   , return PolyPV
--   , liftM IdentPV genIdentifier
  ]

genExpr :: Gen Expr
genExpr = oneof baseExprs
  where
  baseExprs = 
    [ liftM (Num . fromIntegral) genNat
    , liftM Str (genString 5)
-- , liftM QualNameE genQualName fixme:implement
    ]

genDomainDecl :: [(TypeIdentifier, [(Name,PositionValue)])] -> Gen Stmt
genDomainDecl cs = do
  x <- genClassIdentifier
  (t,ps) <- elements cs
  modifySt $ \st -> st{ localPorts = localPorts st ++ [ (Qual (Ident x) n, pv) | (n,pv) <- ps ] }
  return $ Domain x t [] -- fixme: add arguments


-- genIdentifiers :: Gen [Identifier]
-- genIdentifiers = genListMaxLen stdMaxLen genIdentifier

-- genExprs :: Gen [Expr]
-- genExprs = genListMaxLen stdMaxLen genExpr

-- genTypeValue :: Gen TypeValue
-- genTypeValue = oneof
--   [ liftM QualNameTV genQualName
--   , return PolyTV
--   ]

-- genDirectionValue :: Gen DirectionValue
-- genDirectionValue = oneof
--   [ return InputDV
--   , return OutputDV
--   , return BidirectionalDV
--   , return PolyDV
--   , liftM IdentDV genIdentifier
--   ]

-- genIntegerValue :: Gen IntegerValue
-- genIntegerValue = oneof
--   [ return PolyIV
--   , liftM IntIV genInteger
--   , liftM IdentIV genIdentifier
--   ]

-- genQualNames :: Gen [QualName]
-- genQualNames = genListMaxLen stdMaxLen genQualName

-- genQualName :: Gen QualName
-- genQualName = oneof
--   [ liftM UnQual genName
--   , liftM2 Qual genName genName
--   ]

-- genName :: Gen Name
-- genName = oneof
--   [ liftM TypeIdent genTypeIdentifier
--   , liftM Ident genIdentifier
--   ]
