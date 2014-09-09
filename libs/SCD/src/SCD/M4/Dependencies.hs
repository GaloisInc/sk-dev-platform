{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror -XCPP #-}

{- |
Module      : $Header$
Description : M4 dependency analysis and call-graph utilities
Copyright   : (c) Galois, Inc.

M4 dependency analysis and call-graph utilities
-}

module SCD.M4.Dependencies(P, runP, Build(..), buildM4IdMap, M4IdMap,
  Origin(..), InterfaceType(..), callGraphPolicyModules,
  callGraphPolicy, callGraphSupportDefs, originCallGraph, Caller,
  M4CallGraph, callGraph2Dot, callGraph2Ncol, ppIdOrigin,
  originLayerModule, sortCalls) where

import Data.Map(Map, insert, lookup, empty, singleton,
  fromList, toList, findWithDefault, keysSet, elems)

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Graph(stronglyConnComp, SCC)
import qualified Data.MapSet as MapSet

import SCD.M4.Syntax(Policy(..), SupportDefs, SupportDef(..),
  PolicyModule(..), Interface(..), InterfaceElement(..),
  InterfaceType(..), M4Id, LayerModule, Stmt(..), Implementation(..))

import Data.Foldable(Foldable, mapM_)
import Control.Monad.Error(ErrorT, runErrorT, MonadError, throwError)
import Control.Monad.Identity(Identity, runIdentity)
import Control.Monad.State(StateT, runStateT, MonadState, get, put)
import Control.Monad.Reader(ReaderT, runReaderT, MonadReader, ask, local)
import Control.Arrow(right)

import Prelude hiding (lookup, mapM_)

import Text.PrettyPrint.HughesPJ(render, text, Doc, (<+>), (<>), semi)
import Text.PrettyPrint.Pp(Pp(..), pbraces, dquote, above)
import SCD.M4.PrettyPrint()


data Origin =
   InDefinition (Maybe InterfaceType) LayerModule
 | BuiltIn
  deriving (Eq, Ord, Read, Show)

type M4IdMap = Map M4Id Origin

newtype P r s a = P (ReaderT r (StateT s (ErrorT String Identity)) a)
    deriving (Monad,
#ifndef __HADDOCK__
              MonadError String, MonadState s, MonadReader r,
#endif
              Functor)

runP :: P r s a -> r -> s -> Either String (a,s)
runP (P p) r s = (runIdentity . runErrorT . flip runStateT s . flip runReaderT r) p


buildM4IdMap :: [PolicyModule] -> Either String M4IdMap
buildM4IdMap ms = right snd $
                  runP (mapM_ (\m -> local (const (layerModule m)) (build m)) 
                       ms) undefined empty

add :: M4Id -> Origin -> P r M4IdMap ()
add k v = do
  m <- get
  case lookup k m of
    Nothing -> put (insert k v m)
    Just v' -> throwError $ "SCD.M4.Dependencies.add: duplicate definition of "++show k++": "++ show(v',v)

-- | Build the environment map for template and interfaces
class Build a where
  build :: a -> P LayerModule M4IdMap ()

instance Build PolicyModule where
  build p = build (interface p)

instance Build Interface where
  build (InterfaceModule _ es) = build es

instance (Build a, Foldable l) => Build (l a) where
  build = mapM_ build

instance Build InterfaceElement where
  build (InterfaceElement it _ i _) = ask >>= (add i . InDefinition (Just it))

type Caller = Either M4Id LayerModule
-- | Dependency graph for M4 macro calls (to interfaces and
-- templates).  Represented as a map from caller (name of definition
-- or implementation module) to set of callees.
type M4CallGraph = Map Caller (Set M4Id)

originCallGraph :: M4IdMap -> M4CallGraph -> Map Origin (Set Origin)
originCallGraph m c = fromList [ (callerModule k, Set.map callerId v) 
                               | (k,v) <- toList c ]
  where callerModule (Left i) = callerId i
        callerModule (Right lm) = InDefinition Nothing lm
        callerId :: M4Id -> Origin
        callerId i = findWithDefault BuiltIn i m

callGraphList :: (a -> M4CallGraph) -> [a] -> M4CallGraph
callGraphList b = foldr MapSet.union empty . map b

callGraphPolicy :: Policy -> M4CallGraph
callGraphPolicy p = callGraphPolicyModules (policyModules p) `MapSet.union`
                    callGraphSupportDefs (supportDefs p)

callGraphPolicyModules :: [PolicyModule] -> M4CallGraph
callGraphPolicyModules = callGraphList callGraphPolicyModule

callGraphSupportDefs :: SupportDefs -> M4CallGraph
callGraphSupportDefs = callGraphList callGraphSupportDef

callGraphSupportDef :: SupportDef -> M4CallGraph
callGraphSupportDef (SupportDef i ss) = singleton (Left i) (callGraphStmts ss)

sortCalls :: M4CallGraph -> [SCC M4Id]
sortCalls = stronglyConnComp . prep
   where prep :: M4CallGraph -> [(M4Id,M4Id,[M4Id])]
         prep m = [(i,i,Set.elems s) | (Left i,s) <- toList m]

callGraphPolicyModule :: PolicyModule -> M4CallGraph
callGraphPolicyModule p = 
  callGraphInterface (interface p) `MapSet.union`
  callGraphImplementation (layerModule p) (implementation p)

callGraphInterface :: Interface -> M4CallGraph
callGraphInterface (InterfaceModule _ es) = callGraphList callGraphInterfaceElement es

callGraphImplementation :: LayerModule -> Implementation -> M4CallGraph
callGraphImplementation m (Implementation _ _ ss) = singleton (Right m) (callGraphStmts ss)

callGraphInterfaceElement :: InterfaceElement -> M4CallGraph
callGraphInterfaceElement (InterfaceElement _ _ i ss) = singleton (Left i) (callGraphStmts ss)

callGraphStmts :: [Stmt] -> Set M4Id
callGraphStmts = Set.unions . map callGraphStmt

callGraphStmt :: Stmt -> Set M4Id
callGraphStmt (Tunable _ s1 s2)         = callGraphStmts s1 `Set.union` callGraphStmts s2
callGraphStmt (Optional s1 s2)          = callGraphStmts s1 `Set.union` callGraphStmts s2
callGraphStmt (Ifdef _ s1 s2)           = callGraphStmts s1 `Set.union` callGraphStmts s2
callGraphStmt (Ifndef _ s1)             = callGraphStmts s1
callGraphStmt (RefPolicyWarn _)         = Set.empty
callGraphStmt (Call i' _)               = Set.singleton i'
callGraphStmt (Role _ _)                = Set.empty
callGraphStmt (RoleTransition _ _ _)    = Set.empty
callGraphStmt (RoleAllow _ _)           = Set.empty
callGraphStmt (Attribute _)             = Set.empty
callGraphStmt (Type _ _ _)              = Set.empty
callGraphStmt (TypeAlias _ _)           = Set.empty
callGraphStmt (TypeAttribute _ _)       = Set.empty
callGraphStmt (RangeTransition _ _ _ _) = Set.empty
callGraphStmt (TeNeverAllow _ _)        = Set.empty
callGraphStmt (Transition _ _ _)        = Set.empty
callGraphStmt (TeAvTab _ _ _)           = Set.empty
callGraphStmt (CondStmt _ s1 s2)        = callGraphStmts s1 `Set.union` callGraphStmts s2
callGraphStmt (XMLDocStmt _)            = Set.empty
callGraphStmt (SidStmt _)               = Set.empty
callGraphStmt (FileSystemUseStmt _)     = Set.empty
callGraphStmt (GenFileSystemStmt _)     = Set.empty
callGraphStmt (PortStmt _)              = Set.empty
callGraphStmt (NetInterfaceStmt _)      = Set.empty
callGraphStmt (NodeStmt _)              = Set.empty
callGraphStmt (Define _)                = Set.empty
callGraphStmt (Require _)               = Set.empty
callGraphStmt (GenBoolean _ _ _)        = Set.empty

callGraph2Dot :: Map Origin (Set Origin) -> String
callGraph2Dot m = render $ text "digraph" <+> pbraces (fmap ppNode nodes ++
                                                       fmap ppEdge edges)
  where nodes = Set.toList $
                keysSet m `Set.union` foldr Set.union Set.empty (elems m)
        edges = concat [[(k,v) | v <- Set.elems s] | (k,s) <- toList m]
        ppNode n = quoteNode n <> semi
        ppEdge (k,v) = quoteNode k <+> text "->" <+> 
                       quoteNode v <> semi
        quoteNode n = dquote <> ppOriginLayerModule n <> dquote
    
callGraph2Ncol :: Map Origin (Set Origin) -> String
callGraph2Ncol m = render $ above [ppOriginLayerModule a <+> 
                                   ppOriginLayerModule b | (a,b) <- nodups]
  where edges = concat [[ (k,v) | v <- Set.elems s] | (k,s) <- toList m]
        nodups = ins Set.empty edges
        ins _ [] = []
        ins u ((x,y):l) | x' == y' || Set.member (x',y') u 
                                   || Set.member (y',x') u = ins u l
                        | otherwise                        = (x,y):ins u' l
              where x' = originLayerModule x
                    y' = originLayerModule y
                    u' = Set.insert (x',y') u

ppIdOrigin :: (M4Id,Origin) -> Doc
ppIdOrigin (i,o) = ppOriginType o <+> pp i

instance Pp Origin where
  pp o = ppOriginType o <+> ppOriginLayerModule o

ppOriginLayerModule :: Origin -> Doc
ppOriginLayerModule (InDefinition _ m) = pp m
ppOriginLayerModule BuiltIn            = text "builtin"

ppOriginType :: Origin -> Doc
ppOriginType (InDefinition (Just InterfaceType) _) = text " "
ppOriginType (InDefinition (Just TemplateType) _)  = text "T"
ppOriginType (InDefinition Nothing _)              = text "B"
ppOriginType BuiltIn                               = text "*"

originLayerModule :: Origin -> Maybe LayerModule
originLayerModule (InDefinition _ m) = Just m
originLayerModule BuiltIn            = Nothing
