{-# OPTIONS_GHC -Wall -XGeneralizedNewtypeDeriving #-}
{- |
Module      :  Lobster.Gen.Main
Description :  Generate Lobster from Shrimp / SELinux reference policies.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

The toplevel module for a tool for synthesizing Lobster programs
from Shrimp / SE Linux reference policy modules.

-}
module Main where

import SCD.M4.ModuleFiles
import qualified SCD.M4.Syntax as M4
import SCD.M4.KindCheck as M4 ( kcPolicy, ignoreErrorsKindCheckOptions )
import SCD.M4.Options   as M4 ( defaultOptions )
import SCD.M4.KindInfo ( KindInfo )

import SCD.Lobster.Gen.CoreSyn ( Module, newComment, Name(Name) )
import SCD.Lobster.Gen.CoreSyn.Util ( combineClasses )
import SCD.Lobster.Gen.CoreSyn.Mangle( mangle )
import SCD.Lobster.Gen.CoreSyn.Output
import SCD.Lobster.Gen.Monad
import SCD.Lobster.Gen.Match  ( genLobsterPolicyModule )
import SCD.Lobster.Gen.Perm   ( buildPermissions )

import Control.Monad
import Data.Maybe
import Data.List

import System.Environment
import System.Directory
import System.FilePath
import System.IO

data Options
 = Options
     { optOutDir  :: FilePath
     , optVerbose :: Bool
     }
     
defaultOptions :: Options
defaultOptions = Options
     { optOutDir  = "."
     , optVerbose = True
     }

main :: IO ()
main = do
  (p:o:_ls) <- getArgs 
  let opts = Main.defaultOptions{optOutDir=o}
  putStrLn "gen-lobster"
  genLob opts p
  return ()

genLob :: Options
       -> FilePath
       -> IO ()
genLob opts p = do
  pol <- readPolicy Nothing p
  runLobM (withPolicy pol (genLobster opts))

genLobster :: Options
	   -> LobM ()
genLobster opts = do
  pol <- getPolicy
  setPermissionMap (buildPermissions pol)
--  liftIO (writeFile "x.refs" (unlines $ map show $ M.toList $ xrefs k))
  let k = genKindInfo pol
  withKindInfo k $ mapM_ genLobsterPolicyModule (M4.policyModules pol)
  ms <- getModules
  liftIO (outputModules opts ms)
  ls <- getDecls
  when (not (null ls)) $ liftIO $ do
     putStrLn "Toplevel Lobster declarations:.."
     outputModule opts (Name "top.lst",ls)

outputModules :: Options -> [Module] -> IO ()
outputModules opts ms = do
  flg <- doesDirectoryExist (optOutDir opts)
  if flg
   then genThem
   else do
     ok <- okToCreate
     if not ok
      then putStrLn "OK, output of Lobster code aborted."
      else do
        createDirectoryIfMissing True (optOutDir opts)
	genThem
 where
  genThem = do
   when (optVerbose opts) $ 
     putStrLn ("Outputting Lobster modules into: " ++ optOutDir opts)
   mapM_ (outputModule opts) canon_modules

  okToCreate = do
     putStr ("Output directory " ++ show (optOutDir opts) ++ " does not exist")
     putStr (" - create[yn]? ")
     hFlush stdout
     hSetBuffering stdin NoBuffering
     ch <- hGetChar stdin
     return (ch == 'y')

   -- one or more class may map to the same (file)name, group them together.
  canon_modules = 
   mapMaybe canonGroup $
    groupBy (\ a b -> fst a == fst b) $
     sortBy (\ a b -> compare (fst a) (fst b))
           ms
	       
  canonGroup [] = Nothing
  canonGroup ((x,xs1):ys) = Just
      -- group the decls, drop their (identical) names.
    (x, intercalate [newComment ""] (xs1 : map snd ys))


outputModule :: Options -> Module -> IO ()
outputModule opts (Name m,ds) = do
  let outDir = optOutDir opts
  when (optVerbose opts) $ putStrLn ("Generating... " ++ m)
  createDirectoryIfMissing True (outDir </> takeDirectory m)
  writeFile (outDir </> m) (showLobster (mangle (combineClasses ds)))

genKindInfo :: M4.Policy -> KindInfo
genKindInfo pol = ki
  where
   (ki,_) = kcPolicy opts kcOpts pol []

   opts   = M4.defaultOptions
   kcOpts = ignoreErrorsKindCheckOptions

