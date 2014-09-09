{-# OPTIONS -Wall #-}
module FromSimpleLob where

-- FromSimpleLob transforms simple lobster into core lobster.  To accomplish this it desugars simple lobster and discards constructs not needed for code generation.  It is syntacticly close to lobster, but is not equivalent.  It is expected that the input lobster is legal when this module is called, e.g. the policy module has already undergone port type checking.

-- LIMITATIONS:
-- Doesn't support the aliasing of names in expressions, e.g. port p; q = p;
-- Doesn't support different names in new expressions, e.g. x = new "y" ... ;
-- Doesn't support empty string in new expressions, e.g. x = new "" .. ; -- This should directly include the code for the new class(?)
-- The new expression can only be used when assigning to a variable, e.g. x = new ... ;
-- Doesn't support identifier position values, e.g. { position = x }

import qualified AbsSimpleLob as S
import qualified AbsCoreLob as C

fromIdentifier :: S.Identifier -> C.Identifier
fromIdentifier (S.Identifier s) = C.Identifier s

fromTypeIdentifier :: S.TypeIdentifier -> C.TypeIdentifier
fromTypeIdentifier (S.TypeIdentifier s) = C.TypeIdentifier s

fromPolicy :: S.Policy -> C.Policy
fromPolicy (S.Policy tlds ss) = C.Policy (toPolicyName tlds) (toVersion tlds) $ fromStmts ss
 

toPolicyName :: [S.TLDecl] -> C.Identifier
toPolicyName tlds = case [ i | S.PolicyName i <- tlds ] of
  [] -> C.Identifier "module"
  [i] -> fromIdentifier i
  _ -> error "multiple policy name declarations"

toVersion :: [S.TLDecl] -> C.Version
toVersion tlds = case [ (x,y) | S.Version x y <- tlds ] of
  [] -> C.Version 1 0
  [(x,y)] -> C.Version x y
  _ -> error "multiple version declarations"

fromStmts :: [S.Stmt] -> [C.Stmt]
fromStmts ss = concatMap fromStmt ss

fromStmt :: S.Stmt -> [C.Stmt]
fromStmt x = case x of
  S.Class t ys ss ->
    [C.Class (fromTypeIdentifier t) (map fromIdentifier ys) (fromStmts ss)]
  S.TypeS _ -> [] -- types are ignored in core
  S.Port i mpdt mpdc -> -- types are ignored in core
    C.Port pv i1 : [ C.Coupler i1 qn
                  | qn <- fromMPortDeclarationConnection mpdc ]
    where
    i1 = fromIdentifier i
    pv = fromMPortDeclarationType mpdt
  S.Domain i t es -> [C.Domain (fromIdentifier i) (fromTypeIdentifier t) (map fromExpr es)]
  S.Assign i e -> [C.Assign (fromIdentifier i) (fromExpr e)]
  S.Connection qns1 _ qns2 -> -- connection direction is ignored in core
    [ fromConnection qna qnb | qna <- qns1, qnb <- qns2 ]

fromConnection :: S.QualName -> S.QualName -> C.Stmt
fromConnection qn1 qn2 = case (qn1,qn2) of
  (S.UnQual n1, S.UnQual n2) -> error $ "unsupported connection:" ++ show (n1,n2)
  (S.Qual n1 m1, S.UnQual n2) -> C.Coupler (nameToIdentifier n2) (C.Qual (nameToIdentifier n1) (nameToIdentifier m1))
  (S.UnQual n1, S.Qual n2 m2) -> C.Coupler (nameToIdentifier n1) (C.Qual (nameToIdentifier n2) (nameToIdentifier m2))
  (S.Qual n1 m1, S.Qual n2 m2) ->
    C.Adapter (C.Qual (nameToIdentifier n1) (nameToIdentifier m1)) (C.Qual (nameToIdentifier n2) (nameToIdentifier m2))

fromExpr :: S.Expr -> C.Expr
fromExpr x = case x of
  S.QualNameE _ -> error $ "qualified name unsupported here:" ++ show x
  S.Num i -> C.Num i
  S.Str s -> C.Str s

nameToIdentifier :: S.Name -> C.Identifier
nameToIdentifier n = case n of
  S.Ident i -> fromIdentifier i
  S.TypeIdent t -> error $ "type identifier not supported here:" ++ show t

fromMPortDeclarationConnection :: S.MPortDeclarationConnection -> [C.QualIdent]
fromMPortDeclarationConnection x = case x of
  S.EmptyPDC -> []
  S.FullPDC _ qns -> map qualNameToQualIdent qns

qualNameToQualIdent :: S.QualName -> C.QualIdent
qualNameToQualIdent qn = case qn of
  S.UnQual n -> error $ "unqualified name not supported here:" ++ show n
  S.Qual n1 n2 -> C.Qual (nameToIdentifier n1) (nameToIdentifier n2)

fromMPortDeclarationType :: S.MPortDeclarationType -> C.PositionValue
fromMPortDeclarationType mpdt = case mpdt of
  S.EmptyPDT -> C.Unknown
  S.FullPDT mptc -> fromMPortTypeConstraints mptc

fromMPortTypeConstraints :: [S.PortTypeConstraint] -> C.PositionValue
fromMPortTypeConstraints ptcs = case [ fromPositionValue pv | S.Position pv <- ptcs ] of
  [] -> C.Unknown
  [pv] -> pv
  _ -> error $ "multiple position values given:" ++ show ptcs

fromPositionValue :: S.PositionValue -> C.PositionValue
fromPositionValue pv = case pv of
  S.Subject -> C.Subject
  S.Object -> C.Object
  S.PolyPV -> C.Unknown
  S.IdentPV x -> error $ "unsupported position value:" ++ show x
