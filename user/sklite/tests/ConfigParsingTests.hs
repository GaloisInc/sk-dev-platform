module ConfigParsingTests
    ( tests
    )
where

import Control.Applicative
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, cases)

import Sklite.Config
import Sklite.Types

tests :: Test
tests =
    testGroup "Config parsing" $
                  [ testGroup "Expected failures" $
                              mkFailingTestCase <$> failureCases
                  , testGroup "Expected successes" $
                              mkPassingTestCase <$> successCases
                  ]

mkFailingTestCase :: (String, String, String) -> Test
mkFailingTestCase (desc, input, errorMessage) =
    testCase desc $
             case parseLayoutConfig input of
               Left e -> assertEqual "Parsing failed as expected but was the wrong failure"
                         errorMessage e
               Right _ -> assertFailure "Parsing should have failed but succeeded instead"

mkPassingTestCase :: (String, String, Layout) -> Test
mkPassingTestCase (desc, input, expected) =
    testCase desc $
             case parseLayoutConfig input of
               Left e -> assertFailure $ "Parsing should have succeeded but failed with: " ++ e
               Right actual -> assertEqual "Parsing succeeded as expected but yielded an unexpected layout"
                               expected actual

failureCases :: [(String, String, String)]
failureCases =
    [ ( "Not an XML document"
      , ""
      , "No root element found"
      )
    , ( "XML document with invalid root element"
      , "<foo></foo>"
      , "No root element found"
      )
    , ( "Root missing bandwidth"
      , "<layout></layout>"
      , "Required attribute \"bandwidth\" missing from element \"layout\""
      )
    , ( "Root has invalid bandwidth"
      , "<layout bandwidth=\"foo\"></layout>"
      , "Value of \"bandwidth\" attribute must be an integer"
      )
    , ( "Too many args elements"
      , "<layout bandwidth=\"70\"><cell name=\"foo\" user=\"u\" program=\"prog\" runtime=\"10000\" period=\"10000\"><args><arg value=\"foo\"></args><args><arg value=\"bar\"></args></layout>"
      , "At most one 'args' element permitted in each cell"
      )
    , ( "Cell missing runtime"
      , "<layout bandwidth=\"90\"><cell name=\"foo\" user=\"u\" program=\"prog\" period=\"100000\"/></layout>"
      , "Required attribute \"runtime\" missing from element \"cell\""
      )
    , ( "Cell missing period"
      , "<layout bandwidth=\"90\"><cell name=\"foo\" user=\"u\" program=\"prog\" runtime=\"100000\"/></layout>"
      , "Required attribute \"period\" missing from element \"cell\""
      )
    , ( "Cell has invalid runtime"
      , "<layout bandwidth=\"90\"><cell name=\"foo\" user=\"u\" program=\"prog\" runtime=\"foo\" period=\"100000\"/></layout>"
      , "Value of \"runtime\" attribute must be an integer"
      )
    , ( "Cell has invalid period"
      , "<layout bandwidth=\"90\"><cell name=\"foo\" user=\"u\" program=\"prog\" runtime=\"100000\" period=\"foo\"/></layout>"
      , "Value of \"period\" attribute must be an integer"
      )
    , ( "Segment size invalid"
      , "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
        \  <segment name=\"foo\" size=\"foo\" />\n\
        \</layout>"
      , "Value of \"size\" attribute must be an integer"
      )
    , ( "Cell entry address invalid"
      , "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
        \  <cell name=\"foo\" program=\"prog\" entryAddr=\"bogus\" period=\"1\" runtime=\"1\" user=\"joe\">\n\
        \</layout>"
      , "Value of \"entryAddr\" attribute must be an integer"
      )
    , ( "Cell missing binary"
      , "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
        \  <cell name=\"foo\" program=\"prog\" entryAddr=\"0x20000\" period=\"1\" runtime=\"1\" user=\"joe\">\n\
        \</layout>"
      , "Required attribute \"binary\" missing from element \"cell\""
      )
    , ( "Invalid segment map address"
      , "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
        \  <segment name=\"foo\" size=\"4096\" />\n\
        \  <cell name=\"cell\" program=\"prog\" user=\"joe\" period=\"10000\" runtime=\"10000\">\n\
        \    <use-segment name=\"foo\" alias=\"bar\" privileges=\"ro\" mapto=\"invalid\" />\n\
        \  </cell>\n\
        \</layout>"
      , "Value of \"mapto\" attribute must be an integer"
      )
    , ( "Invalid segment privileges"
      , "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
        \  <segment name=\"foo\" size=\"4096\" />\n\
        \  <cell name=\"cell\" program=\"prog\" user=\"joe\" period=\"10000\" runtime=\"10000\">\n\
        \    <use-segment name=\"foo\" alias=\"bar\" privileges=\"bogus\" />\n\
        \  </cell>\n\
        \</layout>"
      , "Invalid privilege string: \"bogus\", must be one of [\"ro\",\"rw\",\"wo\"]"
      )

    -- Configs with layouts
    , ( "Unidirectional channel with missing from"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel name=\"chan\" slots=\"4\" msgsize=\"1024\" to=\"c\" />\n\
        \</layout"
      , "Required attribute \"from\" missing from element \"channel\""
      )
    , ( "Unidirectional channel with missing to"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel name=\"chan\" slots=\"4\" msgsize=\"1024\" from=\"c\" />\n\
        \</layout"
      , "Required attribute \"to\" missing from element \"channel\""
      )
    , ( "Unidirectional channel with missing msgsize"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel name=\"chan\" slots=\"4\" to=\"c\" from=\"c\" />\n\
        \</layout"
      , "Required attribute \"msgsize\" missing from element \"channel\""
      )
    , ( "Unidirectional channel with invalid msgsize"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel name=\"chan\" slots=\"4\" msgsize=\"invalid\" to=\"c\" from=\"c\" />\n\
        \</layout"
      , "Value of \"msgsize\" attribute must be an integer"
      )
    , ( "Unidirectional channel with missing slots"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel name=\"chan\" msgsize=\"1024\" to=\"c\" from=\"c\" />\n\
        \</layout"
      , "Required attribute \"slots\" missing from element \"channel\""
      )
    , ( "Unidirectional channel with invalid slots"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel name=\"chan\" slots=\"invalid\" msgsize=\"1024\" to=\"c\" from=\"c\" />\n\
        \</layout"
      , "Value of \"slots\" attribute must be an integer"
      )
    , ( "Unidirectional channel with missing name"
      , "<layout bandwidth=\"100\" baseDir=\"foo\">\n\
        \  <channel msgsize=\"1024\" slots=\"4\" to=\"c\" from=\"c\" />\n\
        \</layout"
      , "Required attribute \"name\" missing from element \"channel\""
      )
    ]

successCases :: [(String, String, Layout)]
successCases =
    [ ("Valid root", validRootConfig, validRootLayout)
    , ("Defined segments", definedSegmentsConfig, definedSegmentsLayout)
    , ("Cells", cellsConfig, cellsLayout)
    , ("Cells with used segments", cellsSegmentsConfig, cellsSegmentsLayout)
    , ("Cell receives arguments", cellWithArgs, cellWithArgsLayout)
    , ("Channel", unidirChannel, unidirChannelLayout)
    , ("Channel with overwriting (1)", unidirChannelOverwrite "1", unidirChannelOverwriteLayout True)
    , ("Channel with overwriting (2)", unidirChannelOverwrite "yes", unidirChannelOverwriteLayout True)
    , ("Channel with overwriting (3)", unidirChannelOverwrite "true", unidirChannelOverwriteLayout True)
    , ("Channel with overwriting (4)", unidirChannelOverwrite "0", unidirChannelOverwriteLayout False)
    , ("Channel with overwriting (5)", unidirChannelOverwrite "no", unidirChannelOverwriteLayout False)
    , ("Channel with overwriting (6)", unidirChannelOverwrite "false", unidirChannelOverwriteLayout False)
    ]

unidirChannel :: String
unidirChannel =
    "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
    \  <channel name=\"chan\" from=\"c1\" to=\"c2\" msgsize=\"1024\" slots=\"4\" />\n\
    \</layout>"

unidirChannelLayout :: Layout
unidirChannelLayout =
    let c = Channel { chanFrom = "c1"
                    , chanTo = "c2"
                    , chanName = "chan"
                    , chanMsgSize = 1024
                    , chanMsgSlots = 4
                    , chanOverwrite = False
                    }
    in validRootLayout { layoutChannels = [c]
                       }

unidirChannelOverwrite :: String -> String
unidirChannelOverwrite oStr =
    "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
    \  <channel overwrite=\"" ++ oStr ++ "\" name=\"chan\" from=\"c1\" to=\"c2\" msgsize=\"1024\" slots=\"4\" />\n\
    \</layout>"

unidirChannelOverwriteLayout :: Bool -> Layout
unidirChannelOverwriteLayout overwrite =
    let c = Channel { chanFrom = "c1"
                    , chanTo = "c2"
                    , chanName = "chan"
                    , chanMsgSize = 1024
                    , chanMsgSlots = 4
                    , chanOverwrite = overwrite
                    }
    in validRootLayout { layoutChannels = [c]
                       }

validRootConfig :: String
validRootConfig =
    "<layout bandwidth=\"70\" baseDir=\"foo\"></layout>"

validRootLayout :: Layout
validRootLayout =
    Layout [] [] 70 []

cellWithArgs :: String
cellWithArgs =
    "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
    \  <cell name=\"cell\" user=\"u\" program=\"prog\" runtime=\"70000\" period=\"100000\">\n\
    \    <args>\n\
    \      <arg value=\"one\"/>\n\
    \      <arg value=\"two\"/>\n\
    \      <arg value=\"three\"/>\n\
    \    </args>\n\
    \  </cell>\n\
    \</layout>"

cellWithArgsLayout :: Layout
cellWithArgsLayout =
    let cs = [c1]
        c1 = Cell "cell" "u" "prog" CellMain 70000 100000 [] ["one", "two", "three"] []
    in Layout cs [] 70 []

definedSegmentsConfig :: String
definedSegmentsConfig =
    "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
    \  <segment name=\"foo\" size=\"4096\" />\n\
    \</layout>"

definedSegmentsLayout :: Layout
definedSegmentsLayout =
    let r = SharedMemoryRegion 4096 "foo"
    in validRootLayout { sharedMemoryRegions = [r] }

cellsConfig :: String
cellsConfig =
    "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
    \  <segment name=\"foo\" size=\"4096\" />\n\
    \  <cell name=\"cell1\" program=\"prog1\" user=\"joe\" runtime=\"50000\" period=\"100000\" />\n\
    \  <cell name=\"cell2\" program=\"prog2\" user=\"joe\" runtime=\"30000\" period=\"100000\" />\n\
    \  <cell name=\"cell3\" program=\"prog3\" user=\"joe\" binary=\"prog.bin\" runtime=\"10000\" period=\"100000\" entryAddr=\"0x10000\" size=\"0x20000\"/>\n\
    \  </cell>\n\
    \</layout>"

cellsLayout :: Layout
cellsLayout =
    let c1 = Cell "cell1" "joe" "prog1" CellMain 50000 100000 [] [] []
        c2 = Cell "cell2" "joe" "prog2" CellMain 30000 100000 [] [] []
        c3 = Cell "cell3" "joe" "prog3" (RawBinary 0x10000 "prog.bin" 0x20000) 10000 100000 [] [] []
    in definedSegmentsLayout { layoutCells = [c1, c2, c3] }

cellsSegmentsConfig :: String
cellsSegmentsConfig =
    "<layout bandwidth=\"70\" baseDir=\"foo\">\n\
    \  <segment name=\"foo\" size=\"4096\" />\n\
    \  <cell name=\"cell\" program=\"prog\" user=\"joe\" runtime=\"50000\" period=\"100000\">\n\
    \    <use-segment name=\"foo\" alias=\"bar1\" privileges=\"ro\" />\n\
    \    <use-segment name=\"foo\" alias=\"bar2\" privileges=\"rw\" mapto=\"0xdeadbeef\" />\n\
    \    <use-segment name=\"foo\" alias=\"bar3\" privileges=\"wo\" />\n\
    \  </cell>\n\
    \  </cell>\n\
    \</layout>"

cellsSegmentsLayout :: Layout
cellsSegmentsLayout =
    let c = Cell "cell" "joe" "prog" CellMain 50000 100000 [a1, a2, a3] [] []
        a1 = SharedMemoryAccess MemReadOnly "foo" "bar1" Nothing
        a2 = SharedMemoryAccess MemReadWrite "foo" "bar2" (Just 0xdeadbeef)
        a3 = SharedMemoryAccess MemWriteOnly "foo" "bar3" Nothing
    in definedSegmentsLayout { layoutCells = [c] }
