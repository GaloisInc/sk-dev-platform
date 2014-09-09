
import qualified SCD.SELinux.Test.Parser as SELinuxParser
import qualified SCD.SELinux.Test.Symbol as Symbol
import qualified SCD.SELinux.Test.Authorize as Authorize
import qualified SCD.M4.Test.Parser as M4Parser
import qualified SCD.M4.Test.Dependencies as Dependencies
import qualified SCD.M4.Test.KindCheck as KindCheck
import qualified SCD.M4.Test.HTML as HTML
import qualified SCD.M4.Test.ErrorSuite as ErrorSuite
import qualified SCD.M4.Test.QuickLobster as QuickLobster

import System.FilePath((</>),takeExtension)
import System.Environment(getArgs)
import Control.Monad(when)
import System.Directory(getDirectoryContents)
import Control.Monad(liftM)

import Test.Framework (Test, defaultMain, testGroup)

referencePolicyFile :: String
referencePolicyFile = Authorize.testDirectory </> "policy.conf"

policyDirectory :: String
policyDirectory = "../../data/Reference-Policy/refpolicy"

modulesDirectory :: String
modulesDirectory = policyDirectory </> "modules"

testappDirectory :: String
testappDirectory = "../SELinux/testapp"

lobsterExamplePoliciesDirectory :: String
lobsterExamplePoliciesDirectory = "testsrc/SCD/Lobster/Test/examples"

htmlDirectory :: String
htmlDirectory = "dist/html"

lobsterDirectory :: String
lobsterDirectory = "dist/quickLobster"

testappPolicy :: String
testappPolicy = testappDirectory </> "testapp.lsr"

getLobsterExamplePolicies :: IO [(Bool,FilePath)]
getLobsterExamplePolicies = do
    fns <- liftM (filter ((==) ".lsr" . takeExtension)) $
      getDirectoryContents lobsterExamplePoliciesDirectory
    return [ (take 5 fn == "error", lobsterExamplePoliciesDirectory </> fn) | fn <- fns ]

getLobsterPolicies :: IO [(Bool,FilePath)]
getLobsterPolicies = do
    fns <- getLobsterExamplePolicies
    return $ fns ++ [(False,testappPolicy)]

errorSuiteDirectory :: String
errorSuiteDirectory = "testsrc/SCD/M4/Test/ErrorSuite"

-- main :: IO ()
-- main =
--     do as <- getArgs
--        let cond s m = when (null as || s `elem` as) $ m >> return ()
--        -- cond "symbion" Symbion.checks
--        -- cond "lobster"      $ do
--        --   lobsterPolicies <- getLobsterPolicies
--        --   Policy.checks lobsterPolicies
--        cond "errors"       $ ErrorSuite.checks errorSuiteDirectory
--        cond "authorize" $ do
--          policy <- SELinuxParser.checks referencePolicyFile
--          (policy',symbols) <- Symbol.checks referencePolicyFile policy
--          Authorize.checks referencePolicyFile policy' symbols
--        cond "m4parser"     $ M4Parser.checks modulesDirectory
--        cond "dependencies" $ Dependencies.checks modulesDirectory
--        cond "kindcheck"    $ KindCheck.checks policyDirectory [testappDirectory </> "testapp"]
--        cond "html"         $ HTML.checks policyDirectory htmlDirectory
--        cond "quicklobster" $ QuickLobster.checks policyDirectory lobsterDirectory

main :: IO ()
main = do
  tests >>= defaultMain
--   SELinuxParser.checks referencePolicyFile
  return ()

tests :: IO [Test]
tests = do authorizeTests <- Authorize.testCases referencePolicyFile
           return [ testGroup "errors" $
                              ErrorSuite.testCases errorSuiteDirectory
                  , testGroup "SELinux policy parser" $
                              SELinuxParser.testCases referencePolicyFile
                  , testGroup "Symbol tests" $
                              Symbol.testCases referencePolicyFile
                  , testGroup "Authorize tests" authorizeTests
--        cond "m4parser"     $ M4Parser.checks modulesDirectory
--        cond "dependencies" $ Dependencies.checks modulesDirectory
--        cond "kindcheck"    $ KindCheck.checks policyDirectory [testappDirectory </> "testapp"]
--        cond "html"         $ HTML.checks policyDirectory htmlDirectory
--        cond "quicklobster" $ QuickLobster.checks policyDirectory lobsterDirectory
                  ]
