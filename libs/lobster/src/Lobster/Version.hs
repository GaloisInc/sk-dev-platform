{-# OPTIONS -cpp #-}

module Lobster.Version where

#include "revision.h"

version :: (Int,Int,Int)
version = (0,0,1)

revision :: Int
revision = SVN_REV

showAbout :: String
showAbout = showVersion ++ " (rev. " ++ show revision ++ ")"

showVersion :: String
showVersion = show a ++ "." ++ show b ++ "." ++ show c
  where (a,b,c) = version
