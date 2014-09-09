{- |
Module      : $Header$
Description : Tests for HTML generation
Copyright   : (c) Galois, Inc.

Tests for HTML generation
-}

module SCD.M4.Test.HTML where

import SCD.M4.Options(defaultOptions, ifdefDeclFile)
import SCD.M4.KindCheck(kcPolicy, defaultKindCheckOptions)
import SCD.M4.ModuleFiles(readPolicy)
import SCD.M4.HTML(defaultHTMLOptions, genHTML, initializeDir)

checks :: FilePath -> FilePath -> IO ()
checks pd outd = do
  p <- readPolicy (ifdefDeclFile defaultOptions) pd
  let (ki, errs) = kcPolicy defaultOptions defaultKindCheckOptions p []
  initializeDir outd
  genHTML defaultOptions defaultHTMLOptions p ki errs outd
