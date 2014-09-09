{- This script replaces symlinks to /bin/busybox with a executable
 - wrapper in order to allow us to set different SELinux labels
 - on different busybox entry points.
 -
 - By using a hard-coded name in each wrapper instead of checking
 - the called name we avoid possible vulnerabilities when a calling
 - user changes the calling name.
 -
 - This script needs to be called with the CC environment variable
 - set and pointing to the target toolchain.
 -
 -}
module Main where

import Control.Monad (when)
import System.FilePath (takeFileName, (</>))
import System.Environment (getArgs, getEnv)
import System.Directory (getDirectoryContents, removeFile)
import System.Process (readProcess)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)

main :: IO ()
main = mapM_ processDir =<< getArgs

processDir :: FilePath -> IO ()
processDir dir = mapM_ (processFile dir) =<< getDirectoryContents dir

processFile :: FilePath -> FilePath -> IO ()
processFile dir fn = do
  let path = dir </> fn
  status <- getSymbolicLinkStatus path
  when (isSymbolicLink status) $ do
    target <- readSymbolicLink path
    when (takeFileName target == "busybox") $ do
      replaceFileWithWrapper path fn

replaceFileWithWrapper :: FilePath -> String -> IO ()
replaceFileWithWrapper path fn = do
  putStrLn ("Replacing " ++ path)
  removeFile path
  cc <- getEnv "CC"
  _ <- readProcess cc ["-x", "c", "-o", path, "-Os", "-"] (genCSource fn)
  return ()

genCSource :: String -> String
genCSource name = unlines
  [ "#include <unistd.h>"
  , "int main(int argc, char **argv, char **envp) {"
  , "  argv[0] = \"" ++ name ++ "\";"
  , "  return execve(\"/bin/busybox\", argv, envp);"
  , "}"
  ]
