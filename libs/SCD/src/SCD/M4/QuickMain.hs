{- |
Module      : $Header$
Description : Test for quick and dirty Lobster generation
Copyright   : (c) Galois, Inc.

Test for quick and dirty Lobster generation from Shrimp to inform visualization design.
-}


module SCD.M4.QuickMain(main) where

import SCD.M4.Options(defaultOptions, ifdefDeclFile)
import SCD.M4.KindCheck(kcPolicy, ignoreErrorsKindCheckOptions)
import SCD.M4.ModuleFiles(readPolicy)
import SCD.M4.QuickLobster(quickLobster)
import System.FilePath(FilePath)

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pd,outd] -> quickGen pd outd
    _ -> do
      putStrLn ("Usage: qlobster <ref-pol-dir> <out-dir>")
      

quickGen :: FilePath -> FilePath -> IO ()
quickGen pd outd = do
  p <- readPolicy (ifdefDeclFile defaultOptions) pd
  let (ki, _) = kcPolicy defaultOptions ignoreErrorsKindCheckOptions p []
  quickLobster p ki outd
