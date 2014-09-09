{-# OPTIONS_GHC -Wall -Werror #-}
module Main(main) where

import Control.DeepSeq
import Lobster.Common

main :: IO ()
main = do
  (options,fns) <- processOptions
  domain <- parseAndInterpretPolicyFiles_ options fns
  seq (rnf domain) $ return ()
