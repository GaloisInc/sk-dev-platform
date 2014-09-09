{-# OPTIONS  -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      :  $Header$
Description :  Architecture-dependent settings.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Rogan Creswick
Stability   :  provisional
Portability :  undetermined


The name of this module is mostly due to historic reasons.
-}


module ExtTools
	( getInstallDataFileName_
	, getInstallDataFileName
	) where

import System.Directory(getCurrentDirectory, findExecutable, doesFileExist,canonicalizePath)
import System.FilePath((</>), takeDirectory,splitDirectories,joinPath)
import System.Posix.Files(readSymbolicLink)
import Paths_lviz

import Foreign(alloca, peek, peekElemOff, Ptr)
import Foreign.C(peekCString, CInt, CString)

getInstallDataFileName_ :: FilePath -> IO FilePath
getInstallDataFileName_ s = do
  mfn <- getInstallDataFileName s
  case mfn of
    Nothing -> error $ "missing data file:" ++ s
    Just fn -> return fn

getInstallDataFileName :: FilePath -> IO (Maybe FilePath)
getInstallDataFileName fn0 = do
  mdir <- getInstallDir
--   putStrLn $ "original install dir:" ++ show mdir
  case mdir of
    Nothing -> error "can't find executable installation directory"
    Just dir -> do
      fn1 <- getDataFileName fn0
      let fn = joinPath $ [dir] ++ (dropWhile (/= "share") $ splitDirectories fn1)
--       putStrLn $ "original data filename:" ++ show fn1
--       putStrLn $ "effective data filename:" ++ show fn
      b <- doesFileExist fn
      if b
        then return $ Just fn
        else return Nothing

getInstallDir :: IO (Maybe String)
getInstallDir = do
  s <- getFullProgName
  mb <- if '/' `elem` s then do x <- getCurrentDirectory
                                return $ Just $ x </> s
                        else findExecutable s
  case mb of
    Nothing -> return Nothing
    Just p -> do
      p1 <- readSymbolicLinks p >>= canonicalizePath
      return $ Just $ takeDirectory $ takeDirectory p1

readSymbolicLinks :: FilePath -> IO FilePath
readSymbolicLinks f = 
  catch (do f' <- (takeDirectory f </>) `fmap` readSymbolicLink f
            if f == f' then return f
                       else readSymbolicLinks f')
        (const (return f))

-- System.Environment.getProgName doesn't do what we want
getFullProgName :: IO String
getFullProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     peek p_argv >>= flip peekElemOff 0 >>= peekCString

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
