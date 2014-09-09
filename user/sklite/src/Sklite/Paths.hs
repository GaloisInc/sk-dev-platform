module Sklite.Paths
    ( regionFilename
    , driverHfilename
    , driverCfilename
    , driverOfilename
    , driverBasename
    )
where

import Sklite.Types

regionFilename :: SharedMemoryRegion -> String
regionFilename r = "region_" ++ regionName r ++ ".mem"

driverHfilename :: Cell -> String
driverHfilename cell = driverBasename cell ++ ".h"

driverCfilename :: Cell -> String
driverCfilename cell = driverBasename cell ++ ".c"

driverOfilename :: Cell -> String
driverOfilename cell = driverBasename cell ++ ".o"

driverBasename :: Cell -> String
driverBasename cell = cellName cell ++ "_driver"
