{-# OPTIONS_GHC -Wall -Werror #-}
module Lobster.SELinux.Test.SELinux where

import Control.Monad
import System.Cmd
import System.Exit
import System.FilePath

import Lobster.Monad
--import qualified Lobster.Abs as Abs
import qualified Lobster.Lex as Lex
import qualified Lobster.Par as Par
import qualified Lobster.ErrM as ErrM
--import qualified SCD.Lobster.Domain as Domain
import qualified Lobster.Policy as Policy
import Lobster.Policy(
  Domain,
  Policy)

import qualified Lobster.SELinux.SELinux as SELinux
import Lobster.SELinux.SELinux(
  SELinux)

parsePolicyFile :: FilePath -> IO Policy
parsePolicyFile filename =
    do chars <- readFile filename
       let toks = Lex.tokens chars
       case Par.pPolicy toks of
         ErrM.Bad e ->
             error ("ERROR: unable to parse Lobster policy file " ++
                    filename ++ ":\n" ++ e)
         ErrM.Ok policy -> return policy

interpretPolicy :: FilePath -> Policy -> IO Domain
interpretPolicy filename policy =
    case runP (Policy.toDomain policy) of
      Right (eexs,domain) -> do
        sequence_ [ putStrLn x | Right x <- eexs ]
        case [ e | Left e <- eexs ] of
          [] -> return domain
          es -> 
            error ("ERROR: symbion errors in Lobster policy file " ++
                   filename ++ ":\n" ++ unlines es)
      Left err ->
          error ("ERROR: couldn't interpret the Lobster policy file " ++
                 filename ++ ":\n" ++ err)

flattenDomain :: FilePath -> Domain -> Domain
flattenDomain filename domain =
    case runP (Policy.flattenDomain domain) of
      Left err ->
          error ("ERROR: couldn't flatten the Lobster policy file " ++
                 filename ++ ":\n" ++ err)
      Right domain' -> domain'

filenameToModule :: FilePath -> String
filenameToModule = takeBaseName

domainToSELinux :: FilePath -> Domain -> SELinux
domainToSELinux filename domain =
    let moduleName = filenameToModule filename in
    case runP (SELinux.toSELinux moduleName domain) of
      Left err ->
          error ("ERROR: couldn't generate native SELinux from the " ++
                 "Lobster policy file " ++ filename ++ ":\n" ++ err)
      Right selinux -> selinux

checkPolicy :: (Bool, FilePath) -> IO ()
checkPolicy (shouldFail, testPolicyFilename) = do
  when (not shouldFail) $ do
    policy <- parsePolicyFile testPolicyFilename
    domain <- interpretPolicy testPolicyFilename policy
    putStr ("SUCCESS: interpreted the Lobster policy file " ++
            testPolicyFilename ++ ":\n" ++ Policy.prettyPrintDomain domain)
    let domain' = flattenDomain testPolicyFilename domain
    putStr ("SUCCESS: flattened the Lobster policy file " ++
            testPolicyFilename ++ ":\n" ++ Policy.prettyPrintDomain domain')
    let selinux = domainToSELinux testPolicyFilename domain'
    putStr ("SUCCESS: generated native SELinux from the " ++
            "Lobster policy file " ++ testPolicyFilename ++ ":\n" ++
            SELinux.prettyPrintSELinux selinux ++ "\n")
    return ()
  runLobster shouldFail testPolicyFilename

runSystemCmd :: Bool -> String -> IO ()
runSystemCmd shouldFail cmd = do
  ec <- system cmd
  case (shouldFail,ec) of
    (False, ExitFailure i) -> error $ "unable to execute command:" ++ cmd ++ ":(" ++ show i ++ ")"
    (True, ExitSuccess) -> error $ "command succeeded but it should have resulted in an error:" ++ cmd
    _ -> return ()

outputFilePath :: FilePath -> FilePath
outputFilePath fn = takeDirectory fn </> filenameToModule fn

runDiff :: FilePath -> String -> IO ()
runDiff fn ext = do
    let fn1 = outputFilePath fn ++ ext
    let fn2 = fn1 ++ ".gld"
    let cmd = "diff -c " ++ fn2 ++ " " ++ fn1
    putStrLn $ "\nComparing " ++ ext ++ " output:\n" ++ cmd ++ "\n"
    runSystemCmd False cmd

runLobster :: Bool -> FilePath -> IO ()
runLobster shouldFail fn = do
    let cmd = "dist/build/lobster-selinux/lobster-selinux -o " ++
              outputFilePath fn ++ " " ++ fn
    putStrLn $ "\nRunning lobster:\n" ++ cmd ++ "\n"
    runSystemCmd shouldFail cmd
    when (not shouldFail) $ do
      runDiff fn ".fc"
      runDiff fn ".te"

checks :: [(Bool,FilePath)] -> IO ()
checks xs =
    do putStrLn "\nBegin tests of the Lobster compilation"
       mapM_ checkPolicy xs
       putStrLn "End tests of the Lobster compilation"
