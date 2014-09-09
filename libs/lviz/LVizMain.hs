{-# OPTIONS_GHC -Wall #-}
module LVizMain where

import GUI
import Lobster.Common

main :: IO ()
main = do
  (options,fns) <- processOptions
  guiMain $ initCfg options fns
