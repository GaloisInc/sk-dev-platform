{-# OPTIONS -Wall #-}

module Main where

import qualified FromSimpleLob as S
import qualified FromCoreLob as C

import LexSimpleLob
import ParSimpleLob
import AbsSimpleLob
import ErrM

import System
import Text.PrettyPrint

main :: IO ()
main = do
  args <- getArgs
  mapM_ process args

process :: FilePath -> IO ()
process fn = do
  s <- readFile fn
  let ts = myLexer s
  let ast0 = myParse ts
  let ast1 = S.fromPolicy ast0
  let ast2 = C.fromPolicy ast1
  writeFile "module.te.m4" $ render $ C.ppPolicyTE ast2
  writeFile "module.fc.m4" $ render $ C.ppPolicyFC ast2

myParse :: [Token] -> Policy
myParse ts = case pPolicy ts of
  Bad s -> error $ "parse error:" ++ s
  Ok ast -> ast
