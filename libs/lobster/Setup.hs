import Distribution.Simple
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils ( rawSystemExit, warn, debug
                                 , findProgramVersion, notice )
import Distribution.Verbosity ( Verbosity )

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          removeFile )
import System.Cmd       ( rawSystem )


main = do
  defaultMainWithHooks simpleUserHooks
       { preBuild = \a b -> generateBnfc a b >> preBuild simpleUserHooks a b
       , preClean = \a b -> cleanBnfc a b >> preClean simpleUserHooks a b
       , hookedPrograms = [bnfcProgram] }

bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc") {
  programFindVersion = findProgramVersion "--numeric-version" id
  }

bnfcFiles :: [FilePath]
bnfcFiles = [ "src/Lobster/Abs.hs"
            , "src/Lobster/Doc.tex"
            , "src/Lobster/Doc.txt"
            , "src/Lobster/ErrM.hs"
            , "src/Lobster/Lex.x"
            , "src/Lobster/Par.y"
            , "src/Lobster/Print.hs"
            , "src/Lobster/Skel.hs"
            , "src/Lobster/Test.hs"
            ]

cleanBnfc :: Args -> CleanFlags -> IO ()
cleanBnfc _ flags = do
  let verbosity = (fromFlag $ cleanVerbosity flags)
  removeBnfcFiles verbosity

generateBnfc :: Args -> BuildFlags -> IO ()
generateBnfc _ flags = do
  let verbosity = (fromFlag $ buildVerbosity flags)
  notice verbosity "Running bnfc build"
  removeBnfcFiles verbosity
  -- cd src && bnfc -d Lobster/Lobster.cf
  orig <- getCurrentDirectory
  setCurrentDirectory "src"
  rawSystemExit verbosity "bnfc" ["-d", "Lobster/Lobster.cf"]
  setCurrentDirectory orig

removeBnfcFiles :: Verbosity -> IO ()
removeBnfcFiles verbosity = do
  notice verbosity "Cleaning bnfc files"
  mapM_ (removeFileIfExists verbosity) bnfcFiles

removeFileIfExists :: Verbosity -> FilePath -> IO ()
removeFileIfExists v file = removeFile file `catch`
                            \e -> debug v ("Could not remove file "++file++
                                          " due to exception: "++show e)

