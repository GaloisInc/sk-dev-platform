{- |
Module      :  $Header$
Description :  Evaluates graphs and graph predicates.  Used to implement Symbion assertion language.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Brett Letner
Stability   :  provisional
Portability :  portable

Graph and graph predicate implementation that forms the foundation of the Symbion assertion language.

-}

module Lobster.Symbion where

import Data.List
--import Data.Maybe
import Data.Graph.Inductive
-- import Debug.Trace

-- datatypes

-- LGraph is just a UGraph from Data.Graph.Inductive with a better interface for our purposes.
data LGraph a = LGraph
  { ppLabel :: a -> String
  , nodesGr :: [(Node,a)]
  , uGr :: UGr
  }

-- Datatype for our graph predicates.
data GrPred a
  = IsPathP a a (LGraph a)
  | NoPathP a a (LGraph a)
  | AndP (GrPred a) (GrPred a)
  | OrP (GrPred a) (GrPred a)

-- Datatype for storing the output of our predicate evaluation.  Slightly nicer than Bool.
data Ok = Ok | Err Err deriving Show

data Err
  = EPath String String [String]
  | ENoPath String String
  | EAnd Err Err
  | EOr Err Err
  deriving (Show,Eq)

--------------------------------------------------------------------------------
-- Predicate evaluation.
--------------------------------------------------------------------------------

evalGrPred :: (Eq a) => GrPred a -> Ok
evalGrPred x = case x of
  IsPathP a b g -> evalIsPath a b g
  NoPathP a b g -> evalNoPath a b g
  AndP a b -> evalAndOk (evalGrPred a) (evalGrPred b)
  OrP a b -> evalOrOk (evalGrPred a) (evalGrPred b)

evalIsPath :: (Eq a) => a -> a -> LGraph a -> Ok
evalIsPath a b g = case mPath a b g of
  Nothing -> Err $ ENoPath (ppLabel g a) (ppLabel g b)
  Just _ -> Ok

evalNoPath :: (Eq a) => a -> a -> LGraph a -> Ok
evalNoPath a b g = case mPath a b g of
  Nothing -> Ok
  Just p -> Err $ EPath (ppLabel g a) (ppLabel g b) (map (ppLabel g) p)

deleteNode :: (Eq a) => a -> LGraph a -> LGraph a
deleteNode a g = g{ uGr = delNode (toNode g a) (uGr g) }

deleteNodes :: (Eq a) => [a] -> LGraph a -> LGraph a
deleteNodes xs g = foldr deleteNode g xs

evalAndOk :: Ok -> Ok -> Ok
evalAndOk a b = case (a,b) of
  (Err ea, Err eb) -> Err $ EAnd ea eb
  (Err _, _) -> a
  _ -> b

evalOrOk :: Ok -> Ok -> Ok
evalOrOk a b = case (a,b) of
  (Err ea, Err eb) -> Err $ EOr ea eb
  (Err _, _) -> b
  _ -> a

--------------------------------------------------------------------------------
-- Graph functions.
--------------------------------------------------------------------------------

mkLGraph :: (Eq a) => (a -> String) -> [a] -> [(a,a)] -> LGraph a
mkLGraph pp xs ys = {- trace gStr $ -} LGraph
  { ppLabel = pp
  , nodesGr = zs
  , uGr = mkUGraph (map fst zs) [ (lookupNode pp zs a, lookupNode pp zs b) | (a,b) <- ys ]
  }
  where
  zs = zip [ 0 .. ] (nub (xs ++ map fst ys ++ map snd ys))
--  gStr = "[" ++ unlines [ "(" ++ pp a ++ "," ++ pp b ++ ")" | (a,b) <- ys] ++ "]"

mPath :: (Eq a) => a -> a -> LGraph a -> Maybe [a]
mPath a b g = case esp (toNode g a) (toNode g b) (uGr g) of
  [] -> Nothing
  [p] | toLabel g p == a && toLabel g p == b -> Nothing
  p -> Just $ map (toLabel g) p

toNode :: (Eq a) => LGraph a -> a -> Node
toNode g = lookupNode (ppLabel g) (nodesGr g)

lookupNode :: (Eq a) => (a -> String) -> [(Node,a)] -> a -> Node
lookupNode pp xs l0 = case [ n | (n,l) <- xs, l == l0 ] of
  (n:_) -> n
  [] -> error $ "panic:unknown label:" ++ pp l0 ++ "\n" ++ unlines [ show i ++ ":" ++ pp a | (i,a) <- xs ]

toLabel :: LGraph a -> Node -> a
toLabel g n0 = case [ l | (n,l) <- nodesGr g, n == n0 ] of
  (l:_) -> l
  [] -> error $ "panic:unknown node:" ++ show n0

--------------------------------------------------------------------------------
-- Pretty printers.
--------------------------------------------------------------------------------

ppErrs :: [Err] -> String
ppErrs = unlines . map ppErr

ppErr :: Err -> String
ppErr x = case x of
  EPath a b c ->
    "illegal path from " ++ a ++ " to " ++ b ++ " (e.g. " ++ ppPath c ++ ")" ++ "\n"
  ENoPath a b -> "missing path from " ++ a ++ " to " ++ b ++ "\n"
  EAnd a _ -> ppErr a -- just the first error
  EOr a b -> ppErr a ++ "*OR*\n" ++ ppErr b

ppPath :: [String] -> String
ppPath ns = concat $ intersperse " -> " ns
