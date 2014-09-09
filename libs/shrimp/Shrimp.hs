{-# OPTIONS_GHC -Wall #-}
{- |
Module      : $Header$
Description : Shrimp executable/entry point
Copyright   : (c) Galois, Inc.

Shrimp executable/entry point
-}

module Main where

import System.Environment(getProgName, getArgs)
import SCD.M4.KindCheckPolicy(kcPolicyDoc)
import SCD.M4.KindCheck(KindCheckOptions(..), defaultKindCheckOptions, kcPolicy)
import SCD.M4.ModuleFiles(readPolicy)
import SCD.M4.HTML(HTMLOptions(..), defaultHTMLOptions, genHTML)
import SCD.M4.Options(Options(..), defaultOptions)
import Text.PrettyPrint.HughesPJ(render,Doc)
import System.Directory(createDirectoryIfMissing,canonicalizePath)
import System.Console.GetOpt(getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Exit(exitFailure)

usage :: IO a
usage = do
  p <- getProgName
  putStrLn $ unlines $ [ "Usage:"
                       , p++" [global options] html [--no-sort] <input directory> <output directory>"
                       , "  Generate HTML documentation from policy in <input directory> and write it"
                       , "  to <output directory>."
                       , p++" [global options] kindcheck <input directory> [input modules]"
                       , "  Analyze [input modules] given a policy in <input directory>."
                       , "  If no [input modules] are given, analyze the policy instead."
                       , "Global options:"
                       , "--ifdefs <file>    Read ifdef declarations from <file>"
                       , "--implicit-require Make all symbols implicitly required"
                       , "--xml-errors       Output errors in XML format"
                       , "--no-group         Don't group error messages according to message type"
                       , "Local options:"
                       , "--no-sort          Keep definitions in file order"
                       ]
  exitFailure

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["ifdefs"] (ReqArg (\f o -> o{ ifdefDeclFile = Just f }) "FILE") ""
  , Option [] ["implicit-require"] (NoArg (\o->o{ implicitRequire = True })) ""
  , Option [] ["xml-errors"] (NoArg (\o->o{ xmlErrorOutput = True })) ""
  , Option [] ["no-group"] (NoArg (\o->o{ group = False })) "" 
  ]

htmlOptions :: [OptDescr (HTMLOptions -> HTMLOptions)]
htmlOptions = 
  [ Option [] ["no-sort"] (NoArg (\o->o{ sort = False })) "" ]

kindCheckOptions :: [OptDescr (KindCheckOptions -> KindCheckOptions)]
kindCheckOptions = []

main :: IO ()
main = do
  a <- getArgs
  (os,a') <- checkOpt options defaultOptions a
  case a' of
    "html":r      -> do (htmlOs, r') <- checkOpt htmlOptions defaultHTMLOptions r
                        case r' of 
                          [pd,odir] -> generateHTML os htmlOs pd odir
                          _         -> usage
    "kindcheck":r -> do (kindcheckOs, r') <- checkOpt kindCheckOptions defaultKindCheckOptions r
                        case r' of 
                          (pd:pms) -> kindCheck os kindcheckOs pd pms
                          _        -> usage
    _             -> usage

generateHTML :: Options -> HTMLOptions -> FilePath -> FilePath -> IO ()
generateHTML os hos pd odir = do
  p <- readPolicy (ifdefDeclFile os) =<< canonicalizePath pd
  let (ki, errs) = kcPolicy os defaultKindCheckOptions p []
  createDirectoryIfMissing True odir
  genHTML os hos p ki errs odir

putDocLn :: Doc -> IO ()
putDocLn x = putStrLn (render x)

kindCheck :: Options -> KindCheckOptions -> FilePath -> [FilePath] -> IO ()
kindCheck os kos pd pms = kcPolicyDoc os kos pd pms >>= putDocLn.fst

checkOpt :: [OptDescr (a -> a)] -> a -> [String] -> IO (a,[String])
checkOpt os d args =
  case getOpt RequireOrder os args of
    (f, r, [])   -> return (foldl (flip id) d f, r)
    (_, _, errs) -> do p <- getProgName
                       putStrLn (p ++ ": "++concat errs)
                       usage

