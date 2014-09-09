import System.Directory ( getCurrentDirectory, setCurrentDirectory
                        , getDirectoryContents, copyFile, createDirectoryIfMissing)
import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit, rawSystemStdout, info, warn, debug)
import Distribution.System ( buildOS, OS(Linux) )
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs)



main = defaultMainWithHooks simpleUserHooks
    { preConf = \a b -> compileCheckpolicy a b >> preConf simpleUserHooks a b
    , preClean = \a b -> makeClean libsepolSrcPath a b >> makeClean checkPolicySrcPath a b >> preClean simpleUserHooks a b
    }

-- | The location of the native source, relative to the project root:
checkPolicySrcPath :: FilePath
checkPolicySrcPath = "checkpolicy"

libsepolSrcPath :: FilePath
libsepolSrcPath = "libsepol-2.0.41"

-- | Conditionally compiles libsepol and checkpolicy, dependant on the
-- OS of the build machine.  checkpolicy doesn't build on OS X due to
-- Endianess issues, but it is only used for testing, so we can safely
-- skip it.
compileCheckpolicy :: Args -> ConfigFlags -> IO ()
compileCheckpolicy a b = do
  case buildOS of
    Linux -> do
      make libsepolSrcPath a b
      make checkPolicySrcPath a b
    _     -> return ()


make :: FilePath -> Args -> ConfigFlags -> IO ()
make dir _ flags = do
  let verbosity = (fromFlag $ configVerbosity flags)
  cabalDir <- getCurrentDirectory
  setCurrentDirectory dir
  rawSystemLog verbosity "make" []
  setCurrentDirectory cabalDir

makeClean :: FilePath -> Args -> CleanFlags -> IO ()
makeClean dir _ flags = do
  let verbosity = (fromFlag $ cleanVerbosity flags)
  origDir <- getCurrentDirectory
  setCurrentDirectory dir
  rawSystemLog verbosity "make" ["clean"]
  setCurrentDirectory origDir


-- | Execute a system command, logging the output at `info` level.
rawSystemLog :: Verbosity -> String -> [String] -> IO ()
rawSystemLog v cmd args = rawSystemStdout v cmd args >>= info v
