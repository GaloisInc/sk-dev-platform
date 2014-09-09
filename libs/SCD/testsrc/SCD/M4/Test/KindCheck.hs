{- |
Module      : $Header$
Description : Tests for Shrimp's kind-checking
Copyright   : (c) Galois, Inc.

Tests for Shrimp's kind-checking
-}

module SCD.M4.Test.KindCheck(checks) where

import SCD.M4.Options(defaultOptions)
import SCD.M4.KindCheck(defaultKindCheckOptions)
import SCD.M4.KindCheckPolicy(kcPolicyDoc)
import Text.PrettyPrint.HughesPJ(render)

checks :: FilePath -> [FilePath] -> IO ()
checks pd pmfs = do checkPolicy pd []
                    checkPolicy pd pmfs

checkPolicy :: FilePath -> [FilePath] -> IO ()
checkPolicy pd pmfs = putStrLn =<<
            (render . fst) `fmap` kcPolicyDoc defaultOptions defaultKindCheckOptions pd pmfs
