{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      : $Header$
Description : Tests for M4 dependency analysis
Copyright   : (c) Galois, Inc.

Tests for M4 dependency analysis
-}

module SCD.M4.Test.Dependencies where

import SCD.M4.Dependencies(buildM4IdMap, originCallGraph,
  callGraphPolicyModules, originLayerModule, ppIdOrigin,
  callGraph2Dot, sortCalls, M4IdMap, Origin)

import SCD.M4.CheckTypeStmt(checkInterface)
import SCD.M4.Syntax(PolicyModule, interface, M4Id)
import SCD.M4.PrettyPrint()
import Text.PrettyPrint.Pp(Pp(..), above, pnest)
import Text.PrettyPrint.HughesPJ(render, ($+$), text, sep, (<+>))

import SCD.M4.ModuleFiles(readAllPolicyModules)

import Data.Map(toList, Map)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List.GroupSort(groupSort)
import Data.Graph(SCC(..))

checks :: String -> IO ()
checks d  = do
  ms <- readAllPolicyModules d
  m <- either fail return $ buildM4IdMap ms
  let cg = getCallGraph m ms
  printCheckInterfaces ms
  --printM4IdMap m
  dotCallGraph cg "callgraph.dot"
  --printCallGraph cg
  let sc = sortCalls (callGraphPolicyModules ms)
  putStrLn "Sorted M4 mutually recursive calls:"
  printSortedCalls [cs | CyclicSCC cs <- sc]
  let l = [(k,Set.elems v) | (k,v) <- toList cg]
      a = if null l then "-"
          else show ((fromIntegral (sum (map (length . snd) l)) /
                      fromIntegral (length l)) :: Double)
      mx = reverse (groupSort fst [(length s,k) | (k,s) <- l])
      t = 10
  putStrLn $ "Average number of module dependencies: "++ a
  putStrLn $ "Top ten in number of module dependencies:\n"++
           render (above [(text (show le)) <+> sep (map (pp.snd) s) | (le,s) <- take t mx])


printSortedCalls :: [[M4Id]] -> IO ()
printSortedCalls = do
  putStrLn . render . above . map (sep . map pp)


printM4IdMap :: M4IdMap -> IO ()
printM4IdMap m = do
  let gm = groupSort (originLayerModule . snd) (toList m)
  putStrLn "Declared templates (T) and interfaces:"
  putStrLn $ render $ above [ maybe (text "<builtin>") pp lm $+$
                              pnest (above (map ppIdOrigin l))
                            | (lm,l) <- gm]

printCheckInterfaces :: [PolicyModule] -> IO ()
printCheckInterfaces ms = do
  let ss = concatMap (checkInterface . interface) ms
  putStrLn $ "Number of illegal type declarations in interfaces: " ++
             show (length ss)
  putStrLn $ render $ above [ pp i $+$ pnest (above (map snd s))
                            | (i,s) <- groupSort fst ss]

getCallGraph :: M4IdMap -> [PolicyModule] -> Map Origin (Set Origin)
getCallGraph m ms = originCallGraph m (callGraphPolicyModules ms)

printCallGraph :: Map Origin (Set Origin) -> IO ()
printCallGraph cg = do
  putStrLn "Callgraph:"
  putStrLn $ render $ above
               [ pp i $+$
                 pnest (above (Set.toList s))
               | (i,s) <- toList cg]

dotCallGraph :: Map Origin (Set Origin) -> String -> IO ()
dotCallGraph cg f = writeFile f (callGraph2Dot cg)
