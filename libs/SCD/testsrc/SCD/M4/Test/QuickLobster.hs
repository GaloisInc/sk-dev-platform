{- |
Module      : $Header$
Description : Test for quick and dirty Lobster generation
Copyright   : (c) Galois, Inc.

Test for quick and dirty Lobster generation from Shrimp to inform visualization design.
-}


module SCD.M4.Test.QuickLobster(checks) where

import SCD.M4.Options(defaultOptions, ifdefDeclFile)
import SCD.M4.KindCheck(kcPolicy, ignoreErrorsKindCheckOptions)
import SCD.M4.ModuleFiles(readPolicy)
import SCD.M4.QuickLobster(quickLobster)

checks :: FilePath -> FilePath -> IO ()
checks pd outd = do
  p <- readPolicy (ifdefDeclFile defaultOptions) pd
  let (ki, _) = kcPolicy defaultOptions ignoreErrorsKindCheckOptions p []
  quickLobster p ki outd
