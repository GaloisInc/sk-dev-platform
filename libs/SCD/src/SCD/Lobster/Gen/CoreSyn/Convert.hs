module SCD.Lobster.Gen.CoreSyn.Convert where

import SCD.Lobster.Gen.CoreSyn as C
import Lobster.Abs as A hiding ( Name )
import qualified SCD.Lobster.Syntax as AS

import Data.List

-- Trivial (expansion) of Core into the AST types
-- generated by

transCore :: [C.Decl] -> [A.Statement]
transCore ds = concatMap transDecl ds

transDecl :: C.Decl -> [A.Statement]
transDecl d =
  case d of
    Class (Name nm) ps body -> single $
       A.ClassDeclaration
              (A.ClassId (UIdent nm))
	      (map toId ps)
	      (concatMap transDecl body)

    Port (Name nm) pcs _ -> single $
       A.PortDeclaration
              (A.PortId (LIdent nm))
	      (if null pcs then EmptyPDT else PortTypePDT (map transPC pcs))
	      A.EmptyPDC

    Type t as ->
      map (\ (Name i) -> A.ClassDeclaration (A.ClassId (UIdent i)) [] []) (t:as)

    Domain v (Name f) args -> single $
       A.DomainDeclaration (toId v)
                           (A.ClassId (UIdent f))
			   (map unqualName args)

    Connect a b d -> single $
       A.PortConnection [qualName (portDomain a) (portLabel a)]
                        (transDirConn d)
			[qualName (portDomain b) (portLabel b)]

    Comment{} -> []
 where
  single x = [x]

transDirConn :: Dir -> A.Connection
transDirConn d =
 case d of
   L -> A.RightToLeftConnection
   R -> A.LeftToRightConnection
   N -> A.NeutralConnection

transDirDir :: Dir -> A.Direction
transDirDir d =
 case d of
   L -> A.OutputDirection -- pointing out of the class decl..
   R -> A.InputDirection
   N -> A.BidirectionalDirection

transPosPos :: Bool -> A.Position
transPosPos p =
    case p of
      True -> A.SubjectPosition
      False-> A.ObjectPosition

transPC :: C.PortConstraint -> A.PortTypeConstraint
transPC pc =
  case pc of
    PortDir d ->
        PortTypeConstraint AS.directionFlow
          (A.SomeE (A.DirectionExpression (transDirDir d)))
    PortPos p ->
        PortTypeConstraint AS.positionFlow
          (A.SomeE (A.PositionExpression (transPosPos p)))
    PortType n ->
        PortTypeConstraint AS.typeFlow
          (A.SomeE (unqualName n))

unqualName :: Name -> A.Expression
unqualName i = A.QualNameExpression (UnQual (Ident (toId i)))

qualName :: Name -> Name -> A.Expression
qualName a b =
    A.QualNameExpression (A.Qual (UnQual (Ident (toId a))) (A.Ident (toId b)))

toId :: Name -> Identifier
toId (Name n) = Identifier (LIdent n)