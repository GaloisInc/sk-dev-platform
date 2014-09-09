{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-} 

import SCD.M4.Dependencies(buildM4IdMap, originCallGraph,
  callGraph2Dot, callGraph2Ncol, callGraphPolicyModules, Origin)

import SCD.M4.ModuleFiles(readAllPolicyModules)
import System.Environment(getArgs, getProgName)

import Data.Map(Map)
import Data.Set(Set)

main :: IO ()
main = do
  as <- getArgs
  case as of 
    ["-dot",f] -> dotCallGraph f
    ["-ncol",f] -> ncolCallGraph f
    _ -> usage


getCallGraph :: String -> IO (Map Origin (Set Origin))
getCallGraph d = do
  ms <- readAllPolicyModules d
  m <- either fail return $ buildM4IdMap ms
  return (originCallGraph m (callGraphPolicyModules ms))

dotCallGraph :: String -> IO ()
dotCallGraph d = getCallGraph d >>= (putStrLn . callGraph2Dot) 

ncolCallGraph :: String -> IO ()
ncolCallGraph d = getCallGraph d >>= (putStrLn . callGraph2Ncol) 

usage :: IO ()
usage = do 
  p <- getProgName
  fail (p++": [-dot|-ncol] <reference-policy-dir>")
