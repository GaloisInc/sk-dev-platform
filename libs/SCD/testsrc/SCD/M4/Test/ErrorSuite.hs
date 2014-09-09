{- |
Module      : $Header$
Description : Tests for Shrimp M4 error suite
Copyright   : (c) Galois, Inc.

Tests for Shrimp M4 error suite.  The error suite is an extremely
lightweight version of the reference policy that raises all of the
errors that Shrimp is capable of raising.  This module parses and
kind-checks the error suite policy, and then compares the resulting
parse tree of XML errors versus a golden XML file from the repository.
-}

module SCD.M4.Test.ErrorSuite (checks, testCases) where

import SCD.M4.KindCheck          (defaultKindCheckOptions)
import SCD.M4.KindCheckPolicy    (kcPolicyDoc)
import SCD.M4.ErrorsXml          (markerQN, fileQN)
import SCD.M4.Options            (Options(..), defaultOptions)
import System.Directory          (getCurrentDirectory)
import System.FilePath           (splitPath, joinPath, dropTrailingPathSeparator, (</>))
import Control.Arrow             (second)
import Text.Regex                (mkRegex, subRegex)
import Text.XML.Light            (Content(..), Element(..), Attr(..), CData(..), parseXMLDoc, ppTopElement)

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.API ( Test )
import Test.HUnit hiding ( Test )

testCases :: FilePath -> [Test]
testCases path = [testCase "Golden xml comparison" checkXml]
    where checkXml = do
            cwd <- getCurrentDirectory
            let pd         = cwd </> path
                opts       = defaultOptions{ xmlErrorOutput = True }
                process    = procXML . parseXMLDoc . cleanup

            (_,xml) <- second procXML `fmap` kcPolicyDoc opts defaultKindCheckOptions pd []
            refXml  <- process `fmap` readFile (pd </> "ErrorSuite.golden.xml")

            let render = maybe "" ppTopElement
                failMsg = "FAILED:\n" ++ render xml ++ "\nGolden:\n" ++ render refXml++"\n"

            writeFile "xml.out"   $ render xml
            writeFile "refXml.out" $ render refXml

            -- Compare the parse tree of the golden XML output (after prepending
            -- the appropriate parent path) against the parse tree of the
            -- just-generated XML.  They should be exactly the same.

            assertEqual failMsg xml refXml


-- | Verify the resulting errors from parsing and kind-checking the
-- error suite policy versus the \"golden\" XML file.
checks :: FilePath -> IO ()
checks path = do
  putStrLn "Begin tests for shrimp error generation (ErrorSuite)"

  cwd <- getCurrentDirectory
  let pd         = cwd </> path
      opts       = defaultOptions{ xmlErrorOutput = True }
      process    = procXML . parseXMLDoc . cleanup

  (_,xml) <- second procXML `fmap` kcPolicyDoc opts defaultKindCheckOptions pd []
  refXml  <- process `fmap` readFile (pd </> "ErrorSuite.golden.xml")

  let render = maybe "" ppTopElement
  --writeFile "newxml.out"   $ render xml
  --writeFile "transxml.out" $ render refXml

  -- Compare the parse tree of the golden XML output (after prepending
  -- the appropriate parent path) against the parse tree of the
  -- just-generated XML.  They should be exactly the same.

  putStrLn $ "Shrimp error suite " ++
    if xml == refXml then "passed." else
    "FAILED:\n" ++ render xml ++ "\nGolden:\n" ++ render refXml++"\n"
  putStrLn "End tests for shrimp error generation (ErrorSuite)"
  return ()

-- For some reason, the XML parser considers whitespace characters as
-- part of the CDATA content of the parsed XML, which throws off parse
-- tree comparision.  This function strips the extraneous whitespace.
cleanup :: String -> String
cleanup s = subRegex (mkRegex ">[[:space:]]*<") s "><"

prefix :: String
prefix = "ErrorSuite"

-- | Deletes irrelevant prefix of path to each marker element's file
-- | attribute, and rips out line information.
procXML :: Maybe Element -> Maybe Element
procXML Nothing  = Nothing
procXML (Just e) = Just $ e { elContent = map procXML' (elContent e)
                            , elLine = Nothing
                            }

procXML' :: Content -> Content
procXML' (Elem el) =
  Elem $ el{ elAttribs = attrs
           , elContent = children
           , elLine    = Nothing
           }
  where
    children :: [Content]
    children = map procXML' (elContent el)

    attrs :: [Attr]
    attrs | (elName el == markerQN) = map fixAttr (elAttribs el)
          | otherwise               = elAttribs el

    fixAttr :: Attr -> Attr
    fixAttr a | (attrKey a == fileQN) = a{ attrVal = fixPath (attrVal a) }
              | otherwise = a

    fixPath :: FilePath -> FilePath
    fixPath = joinPath . dropWhile ((/=prefix) . dropTrailingPathSeparator) . splitPath

procXML' (Text cd) = Text $ cd{cdLine = Nothing}
procXML' c         = c
