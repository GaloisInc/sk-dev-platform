{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SCD.Lobster.Symbolic.Combine(combine) where

import Data.Map(Map, insert, findWithDefault)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Lobster.Abs( Policy(..), Statement(..), Expression(..)
                             , QualName(..), Name(..), Identifier, ClassId
                             , Connection)
import Control.Monad.State(StateT, MonadState, runStateT, gets, modify)
import Control.Monad.Identity(Identity, runIdentity)

combine :: [Policy] -> Policy
combine ps = Policy (combineStatements (concat [ss | Policy ss <- ps]))

combineStatements :: [Statement] -> [Statement]
combineStatements ss = concat (run S{ origin = Map.empty
                                    , substitution = Map.empty
                                    , connections = Set.empty
                                    } (mapM combineStatement ss))

newtype M a = M (StateT S Identity a)
  deriving (Monad, MonadState S)

run :: S -> M a -> a
run s (M m) = fst (runIdentity (runStateT m s))

data S = S{ origin :: Map (ClassId,[Expression]) Identifier
          , substitution :: Map Identifier Identifier
          , connections :: Set ([Expression], Connection, [Expression])
          }

combineStatement :: Statement -> M [Statement]
combineStatement dd@(DomainDeclaration d c es) = do
  om <- gets origin
  case Map.lookup (c,es) om of
    Just o  -> do modify (\s -> s{ substitution = insert d o (substitution s) })
                  return []
    Nothing -> do modify (\s -> s{ origin = insert (c,es) d (origin s)})
                  return [dd]
combineStatement (PortConnection e1 c e2) = do
  sm <- gets substitution
  let pca@(e1',c',e2') = (substitute sm e1, c, substitute sm e2)
  cns <- gets connections
  if pca `Set.member` cns then return []
    else do modify (\s -> s{ connections = Set.insert pca (connections s)})
            return [PortConnection e1' c' e2']
combineStatement s =
  error $ "SCD.Lobster.Symbolic.combine: unsupported statement: " ++
          show s

class Substitute e where
  substitute :: Map Identifier Identifier -> e -> e

instance Substitute e => Substitute [e] where
  substitute sm = map (substitute sm)

instance Substitute Expression where
  substitute sm (QualNameExpression q) = QualNameExpression (substitute sm q)
  substitute _ (IntExpression i) = IntExpression i
  substitute _ (StringExpression s) = StringExpression s
  substitute _ (DirectionExpression d) = DirectionExpression d
  substitute _ (PositionExpression p) = PositionExpression p
  substitute sm (ParenExpression e) = ParenExpression (substitute sm e)

instance Substitute QualName where
  substitute _ (UnQual n) = UnQual n
  substitute sm (Qual n1 n2) = Qual (substitute sm n1) n2

instance Substitute Name where
  substitute _ (TypeIdent t) = TypeIdent t
  substitute sm (Ident i) = Ident (findWithDefault i i sm)
