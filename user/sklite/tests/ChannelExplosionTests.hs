module ChannelExplosionTests
    ( tests
    )
where

import Control.Applicative
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, cases)

import Sklite.Types
import Sklite.Layout.Validation

tests :: Test
tests =
    testGroup "Channel explosion tests" $ mkTestCase <$> cases

mkTestCase :: (String, Layout, Layout) -> Test
mkTestCase (desc, input, expected) =
    testCase desc $
      case explodeChannels (ValidatedLayout input) of
         Left e -> assertFailure $ "Expected success but got error: " ++ e
         Right (ExplodedLayout (ValidatedLayout v)) ->
             assertEqual "Expected resulting layout to be equal" expected v

cases :: [(String, Layout, Layout)]
cases = [ ("Single channel"
          , unidir
          , unidirExploded
          )
        , ("Multiple channels between the same cells"
          , unidirMultiple
          , unidirMultipleExploded
          )
        , ("Layout with existing shared memory regions / access"
          , existingShmem
          , existingShmemExploded
          )
        ]

unidir :: Layout
unidir =
    let ch = Channel "c1" "c2" 4096 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 5000 10000 [] [] []
    in Layout [c1, c2] [] 100 [ch]

unidirExploded :: Layout
unidirExploded =
    let ch = Channel "c1" "c2" 4096 1 "chan" False

        -- Reader access:
        a1 = SharedMemoryAccess MemReadOnly "ch_chan_slots" "ch_chan_slots" Nothing
        a2 = SharedMemoryAccess MemReadOnly "ch_chan_writer" "ch_chan_writer" Nothing
        a3 = SharedMemoryAccess MemReadWrite "ch_chan_reader" "ch_chan_reader" Nothing

        -- Writer access:
        a4 = SharedMemoryAccess MemWriteOnly "ch_chan_slots" "ch_chan_slots" Nothing
        a5 = SharedMemoryAccess MemReadWrite "ch_chan_writer" "ch_chan_writer" Nothing
        a6 = SharedMemoryAccess MemReadOnly "ch_chan_reader" "ch_chan_reader" Nothing

        from = Cell "c1" "" "prog" CellMain 5000 10000 [a4, a5, a6] [] []
        to = Cell "c2" "" "prog" CellMain 5000 10000 [a1, a2, a3] [] []

        r1 = SharedMemoryRegion 4096 "ch_chan_slots"
        r2 = SharedMemoryRegion metadataBufSize "ch_chan_reader"
        r3 = SharedMemoryRegion metadataBufSize "ch_chan_writer"
    in Layout [from, to] [r1, r2, r3] 100 [ch]

unidirMultiple :: Layout
unidirMultiple =
    let ch1 = Channel "c1" "c2" 4096 1 "chan1" False
        ch2 = Channel "c1" "c2" 4096 1 "chan2" False
        c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 5000 10000 [] [] []
    in Layout [c1, c2] [] 100 [ch1, ch2]

unidirMultipleExploded :: Layout
unidirMultipleExploded =
    let ch1 = Channel "c1" "c2" 4096 1 "chan1" False
        ch2 = Channel "c1" "c2" 4096 1 "chan2" False

        -- Reader access:
        a1 = SharedMemoryAccess MemReadOnly "ch_chan1_slots" "ch_chan1_slots" Nothing
        a2 = SharedMemoryAccess MemReadOnly "ch_chan1_writer" "ch_chan1_writer" Nothing
        a3 = SharedMemoryAccess MemReadWrite "ch_chan1_reader" "ch_chan1_reader" Nothing

        a4 = SharedMemoryAccess MemReadOnly "ch_chan2_slots" "ch_chan2_slots" Nothing
        a5 = SharedMemoryAccess MemReadOnly "ch_chan2_writer" "ch_chan2_writer" Nothing
        a6 = SharedMemoryAccess MemReadWrite "ch_chan2_reader" "ch_chan2_reader" Nothing

        -- Writer access:
        a7 = SharedMemoryAccess MemWriteOnly "ch_chan1_slots" "ch_chan1_slots" Nothing
        a8 = SharedMemoryAccess MemReadWrite "ch_chan1_writer" "ch_chan1_writer" Nothing
        a9 = SharedMemoryAccess MemReadOnly "ch_chan1_reader" "ch_chan1_reader" Nothing

        a10 = SharedMemoryAccess MemWriteOnly "ch_chan2_slots" "ch_chan2_slots" Nothing
        a11 = SharedMemoryAccess MemReadWrite "ch_chan2_writer" "ch_chan2_writer" Nothing
        a12 = SharedMemoryAccess MemReadOnly "ch_chan2_reader" "ch_chan2_reader" Nothing

        from = Cell "c1" "" "prog" CellMain 5000 10000 [a7, a8, a9, a10, a11, a12] [] []
        to = Cell "c2" "" "prog" CellMain 5000 10000 [a1, a2, a3, a4, a5, a6] [] []

        r1 = SharedMemoryRegion 4096 "ch_chan1_slots"
        r2 = SharedMemoryRegion metadataBufSize "ch_chan1_reader"
        r3 = SharedMemoryRegion metadataBufSize "ch_chan1_writer"

        r4 = SharedMemoryRegion 4096 "ch_chan2_slots"
        r5 = SharedMemoryRegion metadataBufSize "ch_chan2_reader"
        r6 = SharedMemoryRegion metadataBufSize "ch_chan2_writer"
    in Layout [from, to] [r1, r2, r3, r4, r5, r6] 100 [ch1, ch2]

existingShmem :: Layout
existingShmem =
    let ch = Channel "c1" "c2" 4096 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 5000 10000 [a] [] []
        r = SharedMemoryRegion  1234 "region"
        a = SharedMemoryAccess MemReadOnly "region" "a" Nothing
    in Layout [c1, c2] [r] 100 [ch]

existingShmemExploded :: Layout
existingShmemExploded =
    let r = SharedMemoryRegion  1234 "region"
        a = SharedMemoryAccess MemReadOnly "region" "a" Nothing

        ch = Channel "c1" "c2" 4096 1 "chan" False

        -- Reader access:
        a1 = SharedMemoryAccess MemReadOnly "ch_chan_slots" "ch_chan_slots" Nothing
        a2 = SharedMemoryAccess MemReadOnly "ch_chan_writer" "ch_chan_writer" Nothing
        a3 = SharedMemoryAccess MemReadWrite "ch_chan_reader" "ch_chan_reader" Nothing

        -- Writer access:
        a4 = SharedMemoryAccess MemWriteOnly "ch_chan_slots" "ch_chan_slots" Nothing
        a5 = SharedMemoryAccess MemReadWrite "ch_chan_writer" "ch_chan_writer" Nothing
        a6 = SharedMemoryAccess MemReadOnly "ch_chan_reader" "ch_chan_reader" Nothing

        from = Cell "c1" "" "prog" CellMain 5000 10000 [a4, a5, a6] [] []
        to = Cell "c2" "" "prog" CellMain 5000 10000 [a, a1, a2, a3] [] []

        r1 = SharedMemoryRegion 4096 "ch_chan_slots"
        r2 = SharedMemoryRegion metadataBufSize "ch_chan_reader"
        r3 = SharedMemoryRegion metadataBufSize "ch_chan_writer"
    in Layout [from, to] [r, r1, r2, r3] 100 [ch]
