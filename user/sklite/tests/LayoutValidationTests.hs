module LayoutValidationTests
    ( tests
    )
where

import Control.Applicative
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, cases)

import Sklite.Types
import Sklite.Layout.Validation

data Result = Succeeded
            | Failed String
              deriving (Eq, Show)

tests :: Test
tests =
    testGroup "Layout validation tests" $ mkTestCase <$> cases

mkTestCase :: (String, Layout, Result) -> Test
mkTestCase (desc, layout, result) =
    testCase desc $ do
      let validationResult = validateLayout layout
      case result of
        Failed e -> assertEqual "Expected validation failure"
                    (Left e) validationResult
        Succeeded -> case validationResult of
                       Right _ -> return ()
                       Left e -> assertEqual "Expected validation success" (Right()) (Left e)

cases :: [(String, Layout, Result)]
cases = [ ("Missing shared memory region"
          , missingSharedMemoryRegion
          , Failed "No such shared memory regions: [\"missing1\",\"missing2\"]"
          )
        , ( "Bandwidth exceeds value declared in config"
          , bandwidthExceedsMax
          , Failed "Scheduling percentages of cells must not exceed 90.0%"
          )
        , ( "Bandwidth is too high"
          , bandwidthTooHigh
          , Failed "Layout bandwidth exceeds 100%"
          )
        , ( "Cell scheduling runtime exceeds period"
          , runtimeExceedsPeriod
          , Failed "Cell \"c1\" has runtime larger than period"
          )
        , ( "Cell name duplicated"
          , duplicateCellName
          , Failed "All cell names must be unique"
          )
        , ( "Multiple shared memory region references in the same cell"
          , multipleShmemReferences
          , Failed "Cell \"c\" contains multiple references to the same shared memory region"
          )
        , ( "Empty cell names not permitted"
          , emptyCellName
          , Failed "Cell names must be non-empty"
          )
        , ( "Empty cell programs not permitted"
          , emptyCellProgram
          , Failed "Cell programs must be non-empty"
          )
        , ( "Empty memory access aliases not permitted"
          , emptyAccessAlias
          , Failed "Memory access aliases must be non-empty"
          )
        , ( "Duplicate memory access aliases not permitted"
          , duplicateAccessAlias
          , Failed "Memory access aliases must be unique to each cell"
          )
        , ( "Memory access aliases must start with a letter"
          , malformedAccessAlias
          , Failed "Memory access aliases must start with a letter"
          )

        -- Layouts with invalid channels
        , ( "Unidirectional channel with an invalid from end"
          , unidirInvalidFrom
          , Failed "Invalid channel end: invalid"
          )
        , ( "Unidirectional channel with an invalid to end"
          , unidirInvalidTo
          , Failed "Invalid channel end: invalid"
          )
        , ( "Unidirectional channel without unique ends"
          , unidirSameEnds
          , Failed "Channel uses cell \"c1\" for both ends"
          )
        , ( "Unidirectional channels with the same name connected to the same cell(s) (1)"
          , unidirSameName1
          , Failed "All channel names must be unique"
          )
        , ( "Unidirectional channels with the same name connected to the same cell(s) (2)"
          , unidirSameName2
          , Failed "All channel names must be unique"
          )

        -- Layouts with valid channels
        , ( "Valid unidirectional channel"
          , unidirValid
          , Succeeded
          )

        -- Valid layouts
        , ( "Valid layout with no shared memory regions"
          , validLayoutNoRegions
          , Succeeded
          )
        , ( "Valid layout with some shared memory regions unused"
          , validLayoutSomeRegionsUnused
          , Succeeded
          )
        , ( "Valid layout with some shared memory regions used"
          , validLayoutSomeRegionsUsed
          , Succeeded
          )
        , ( "Valid layout with multiple cells using the same program"
          , validLayoutMultiUse
          , Succeeded
          )
        , ( "Valid layout with some regions used"
          , validLayoutMultiSomeRegionsUsed
          , Succeeded
          )
        ]

-- Layouts to validate
missingSharedMemoryRegion :: Layout
missingSharedMemoryRegion =
    let c1 = Cell "c1" "user" "prog1" CellMain 5000 10000 [a] [] []
        c2 = Cell "c2" "user" "prog2" CellMain 5000 10000 [b] [] []
        a = SharedMemoryAccess MemReadOnly "missing1" "a" Nothing
        b = SharedMemoryAccess MemReadOnly "missing2" "b" Nothing
    in Layout [c1, c2] [] 100 []

bandwidthTooHigh :: Layout
bandwidthTooHigh =
    let c1 = Cell "c1" "user" "prog" CellMain 95000 100000 [] [] []
    in Layout [c1] [] 110 []

runtimeExceedsPeriod :: Layout
runtimeExceedsPeriod =
    let c1 = Cell "c1" "user" "prog" CellMain 100000 5000 [] [] []
    in Layout [c1] [] 100 []

bandwidthExceedsMax :: Layout
bandwidthExceedsMax =
    let c1 = Cell "c1" "user" "prog" CellMain 95000 100000 [] [] []
    in Layout [c1] [] 90 []

duplicateCellName :: Layout
duplicateCellName =
    let c1 = Cell "c" "user" "prog1" CellMain 5000 10000 [] [] []
        c2 = Cell "c" "user" "prog2" CellMain 5000 10000 [] [] []
    in Layout [c1, c2] [] 100 []

multipleShmemReferences :: Layout
multipleShmemReferences =
    let c = Cell "c" "user" "prog" CellMain 10000 10000 [a1, a2] [] []
        a1 = SharedMemoryAccess MemReadOnly "region1" "r1" Nothing
        a2 = SharedMemoryAccess MemReadOnly "region1" "r2" Nothing
        region1 = SharedMemoryRegion 4096 "region1"
    in Layout [c] [region1] 100 []

emptyCellName :: Layout
emptyCellName =
    let c = Cell "" "user" "prog" CellMain 10000 10000 [] [] []
    in Layout [c] [] 100 []

emptyCellProgram :: Layout
emptyCellProgram =
    let c = Cell "c1" "user" "" CellMain 10000 10000 [] [] []
    in Layout [c] [] 100 []

{-| `validLayoutNoRegion` in Lobster syntax:

> class C (program) { }
> domain c = C("prog");

-}
validLayoutNoRegions :: Layout
validLayoutNoRegions =
    let c = Cell "c1" "" "prog" CellMain 10000 10000 [] [] []
    in Layout [c] [] 100 []

{-| `validLayoutMulitUse` in Lobster syntax:

> class C1 (program) { }
> class C2 (program) { }
>
> domain c1 = C1("prog");
> domain c2 = C2("prog");

 -}
validLayoutMultiUse :: Layout
validLayoutMultiUse =
    let c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 5000 10000 [] [] []
    in Layout [c1, c2] [] 100 []

{-| `validLayoutSomeRegionsUnused` in Lobster syntax:

> class C1 (program) {
>       port r1;
>       port r2;
> }
>
> domain c1 = C1("prog");

 -}
validLayoutSomeRegionsUnused :: Layout
validLayoutSomeRegionsUnused =
    let c1 = Cell "c1" "" "prog" CellMain 10000 10000 [] [] []
        r1 = SharedMemoryRegion 4096 "r1"
        r2 = SharedMemoryRegion 4096 "r2"
    in Layout [c1] [r1, r2] 100 []

{-| `validLayoutSomeRegionsUsed` in Lobster syntax:

> class C1 (program) {
>       port r1 : { direction = input,  type = MemReadOnly  };
>       port r2 : { direction = output, type = MemWriteOnly };
> }
>
> domain c1 = C1("prog");

 -}
validLayoutSomeRegionsUsed :: Layout
validLayoutSomeRegionsUsed =
    let c1 = Cell "c1" "" "prog" CellMain 10000 10000 [a1, a2] [] []
        a1 = SharedMemoryAccess MemReadOnly "r1" "r1" Nothing
        a2 = SharedMemoryAccess MemWriteOnly "r2" "r2" (Just 0xdeadbeef)
        r1 = SharedMemoryRegion 4096 "r1"
        r2 = SharedMemoryRegion 4096 "r2"
    in Layout [c1] [r1, r2] 100 []

{-| `validLayoutMultiSomeRegionsUsed` in Lobster syntax:

> class C1 (program) {
>       port p1 : { direction = input,  type = MemReadOnly  };
>       port p2 : { direction = output, type = MemWriteOnly };
> }
>
> class C2 (program) {
>       port r1 : { direction = input,  type = MemReadOnly  };
>       port r2 : { direction = output, type = MemWriteOnly };
> }
>
> domain c1 = C1("prog1");
> domain c2 = C2("prog1");
>
> c1.p1 <-- c2.r2;
> c1.p2 --> c2.r1;

-}
validLayoutMultiSomeRegionsUsed :: Layout
validLayoutMultiSomeRegionsUsed =
    let c1 = Cell "c1" "" "prog1" CellMain 8000 10000 [a1, a2] [] []
        c2 = Cell "c2" "" "prog2" CellMain 2000 10000 [a2, a1] [] []
        a1 = SharedMemoryAccess MemReadOnly "r1" "r1" Nothing
        a2 = SharedMemoryAccess MemWriteOnly "r2" "r2" Nothing
        r1 = SharedMemoryRegion 4096 "r1"
        r2 = SharedMemoryRegion 4096 "r2"
    in Layout [c1, c2] [r1, r2] 100 []

emptyAccessAlias :: Layout
emptyAccessAlias =
    let c1 = Cell "c1" "" "prog" CellMain 10000 10000 [a1] [] []
        a1 = SharedMemoryAccess MemReadOnly "r1" "" Nothing
        r1 = SharedMemoryRegion 4096 "r1"
    in Layout [c1] [r1] 100 []

duplicateAccessAlias :: Layout
duplicateAccessAlias =
    let c1 = Cell "c1" "" "prog" CellMain 10000 10000 [a1, a2] [] []
        a1 = SharedMemoryAccess MemReadOnly "r1" "a" Nothing
        a2 = SharedMemoryAccess MemReadOnly "r2" "a" Nothing
        r1 = SharedMemoryRegion 4096 "r1"
        r2 = SharedMemoryRegion 4096 "r2"
    in Layout [c1] [r1, r2] 100 []

malformedAccessAlias :: Layout
malformedAccessAlias =
    let c1 = Cell "c1" "" "prog" CellMain 10000 10000 [a1] [] []
        a1 = SharedMemoryAccess MemReadOnly "r1" "123" Nothing
        r1 = SharedMemoryRegion 4096 "r1"
    in Layout [c1] [r1] 100 []

unidirInvalidFrom :: Layout
unidirInvalidFrom =
    let ch = Channel "invalid" "c1" 1024 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 10000 10000 [] [] []
    in Layout [c1] [] 100 [ch]

unidirInvalidTo :: Layout
unidirInvalidTo =
    let ch = Channel "c1" "invalid" 1024 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 10000 10000 [] [] []
    in Layout [c1] [] 100 [ch]

unidirSameEnds :: Layout
unidirSameEnds =
    let ch = Channel "c1" "c1" 1024 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 10000 10000 [] [] []
    in Layout [c1] [] 100 [ch]

unidirSameName1 :: Layout
unidirSameName1 =
    let ch1 = Channel "c1" "c2" 1024 1 "chan" False
        ch2 = Channel "c1" "c2" 1024 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 5000 10000 [] [] []
    in Layout [c1, c2] [] 100 [ch1, ch2]

unidirSameName2 :: Layout
unidirSameName2 =
    let ch1 = Channel "c1" "c2" 1024 1 "chan" False
        ch2 = Channel "c1" "c3" 1024 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 3000 10000 [] [] []
        c3 = Cell "c3" "" "prog" CellMain 2000 10000 [] [] []
    in Layout [c1, c2, c3] [] 100 [ch1, ch2]

unidirValid :: Layout
unidirValid =
    let ch = Channel "c1" "c2" 1024 1 "chan" False
        c1 = Cell "c1" "" "prog" CellMain 5000 10000 [] [] []
        c2 = Cell "c2" "" "prog" CellMain 5000 10000 [] [] []
    in Layout [c1, c2] [] 100 [ch]
