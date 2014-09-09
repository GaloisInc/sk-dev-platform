{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -cpp #-}
{- |
Module      : $Header$
Description : Working with Shrimp M4 abstract syntax.
Copyright   : (c) Galois, Inc.

Working with Shrimp M4 abstract syntax.
-}
module SCD.M4.Util where

import SCD.SELinux.Syntax as SE
import SCD.M4.Syntax as M4
import Data.List ( intercalate )

elementId :: InterfaceElement -> M4Id
elementId (InterfaceElement _ _ i _) = i

implementationId :: Implementation -> ModuleId
implementationId (Implementation m _ _) = m

layerModule2Identifier :: IsIdentifier i => LayerModule -> i
layerModule2Identifier (l,m) = mkId ((idString l) ++ '_' : idString m)

-- | unravel left-to-right.
splitId :: String -> [String]
splitId xs = 
  case break (=='_') xs of
    (as,[]) -> [as]
    (as,_:bs) -> as : splitId bs

unsplitId :: [String] -> String
unsplitId = intercalate "_"

-- | split up again, but with reversed result;
-- most specific kind/name first.
revSplitId :: String -> [String]
revSplitId = go []
 where
  go acc "" = acc
  go acc xs = 
   case break (=='_') xs of
     (as,[]) -> as:acc
     (as,_:bs) -> go (as : acc) bs

-- | @dropIdSuffix "t" "foo_bar_t"@ returns @"foo_bar"@,
-- snipping out the @t@ suffix. If no such suffix, it
-- returns the 
dropIdSuffix :: String -> String -> String
dropIdSuffix t s = 
  case revSplitId s of
    (x:xs) | t == x -> unsplitId (reverse xs)
    _ -> s

isInstantiatedId :: [String] -> Bool
isInstantiatedId forMe  = bigBucks `any` forMe
 where
  bigBucks ('$':_) = True
  bigBucks _ = False
  
