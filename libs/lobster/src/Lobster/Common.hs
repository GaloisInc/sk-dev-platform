module Lobster.Common
        (
          parseAndInterpretPolicyFiles_
        , parseAndInterpretPolicyFiles
        , addIncludeOptions
        , processOptions
        , Options(..)

        , module Lobster.Policy
        )
where

import System.Console.GetOpt ( getOpt
                             , usageInfo
                             , ArgOrder(..)
                             , OptDescr(..)
                             , ArgDescr(..)
                             )
import System.Environment    ( getProgName, getArgs )
import System.Exit           ( exitFailure )

import Lobster.Policy

data Options = Options
  { includeFiles :: [String]
  , outputFile :: String
  } deriving (Eq, Read, Show, Ord)

parseAndInterpretPolicyFiles_ :: Options -> [FilePath] -> IO Domain
parseAndInterpretPolicyFiles_ options fns = do
  (errs,d) <- parseAndInterpretPolicyFiles options fns
  if null errs
    then return d
    else error $ "ERROR: symbion errors in Lobster policy file(s) " ++ ":\n" ++
           unlines errs

parseAndInterpretPolicyFiles :: Options -> [FilePath] -> IO ([String],Domain)
parseAndInterpretPolicyFiles options fns = do
  let files = includeFiles options ++ fns
  policy <- parsePolicyFiles files
  let (eexs,dom) = interpretPolicy policy
  sequence_ [ putStrLn x | Right x <- eexs ]
  return ([ e | Left e <- eexs ],dom)

processOptions :: IO (Options,[String])
processOptions =
    do args <- getArgs
       case getOpt RequireOrder programOptions args of
         (fs,work,[]) -> return (foldl (flip id) defaultOptions fs, work)
         (_,_,errs) -> do p <- getProgName
                          putStrLn (p ++ ": " ++ concat errs)
                          usage

defaultOptions :: Options
defaultOptions = Options
  { includeFiles = []
  , outputFile = "module"
  }

programOptions :: [OptDescr (Options -> Options)]
programOptions =
    [ Option ['I'] ["include"] (ReqArg addIncludeOptions "include.lsr")
       "Include a lobster file"
    , Option ['o'] ["module"] (ReqArg setOutputOptions "module")
       "Set the name of the output module"
    ]

addIncludeOptions :: String -> Options -> Options
addIncludeOptions file options =
    options {includeFiles = includeFiles options ++ [file]}

setOutputOptions :: String -> Options -> Options
setOutputOptions file options = options {outputFile = file}

usage :: IO a
usage =
    do p <- getProgName
       let header =
             "Usage:\n" ++ p ++ " [-I include.lsr] [-o module] input.lsr ..."
       putStrLn $ usageInfo header programOptions
       exitFailure

parsePolicyFiles :: [String] -> IO Policy
parsePolicyFiles filenames =
    case filenames of
      [] -> return empty
      filename : filenames' ->
        do policy <- parsePolicyFile filename
           policy' <- parsePolicyFiles filenames'
           return (append policy policy')

