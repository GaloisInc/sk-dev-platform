module Sklite.Types
    ( Layout(..)
    , Cell(..)
    , Channel(..)
    , SharedMemoryRegion(..)
    , SharedMemoryAccess(..)
    , AccessType(..)
    , CellRunMethod(..)
    , ScheduleRate
    , ScheduleRuntime
    , ScheduleDeadline

    , addCell
    , chanBufSize
    , cellBandwidth
    )
where

type ScheduleRate = Int
type ScheduleRuntime = Integer
type ScheduleDeadline = Integer

data Layout =
    Layout { layoutCells :: [Cell]
           -- ^The cells of the layout.

           , sharedMemoryRegions :: [SharedMemoryRegion]
           -- ^The shared memory regions available to the layout's
           -- cells.

           , layoutBandwidth :: Double
           -- ^The percentage of total system CPU bandwidth available
           -- to the separation kernel application.

           , layoutChannels :: [Channel]
           -- ^The shared memory channels in the layout.
           }
    deriving (Eq, Show)

data Channel =
    Channel { chanFrom :: String
            -- ^The name of the source cell.

            , chanTo :: String
            -- ^The name of the destination cell.

            , chanMsgSize :: Integer
            -- ^The size of the channel messages in bytes.

            , chanMsgSlots :: Integer
            -- ^The number of message slots in the channel.

            , chanName :: String
            -- ^The name of the channel as it will be called in the
            -- cell program source.

            , chanOverwrite :: Bool
            -- ^Whether the channel should permit overwrites on
            -- writing.
            }
    deriving (Eq, Show)

data CellRunMethod = CellMain
                   -- ^This cell is a compiled cell with a cell_main
                   -- function.
                   | RawBinary { binaryEntryPoint :: Integer
                               , binaryFile :: FilePath
                               , binaryRegionSize :: Integer
                               }
                   -- ^This cell is a raw binary and has the provided
                   -- entry point address and memory segment size.
                   deriving (Eq, Show)

data Cell =
    Cell { cellName :: String
         -- ^A unqiue name for the cell.

         , cellUser :: String
         -- ^The username of the unix account under which the cell
         -- should run.

         , cellProgram :: FilePath
         -- ^The program running in this cell.

         , cellRunMethod :: CellRunMethod
         -- ^The method of executing this cell.

         , cellScheduleRuntime :: Double
         -- ^The runtime, in microseconds, that the cell needs to run
         -- every 'period' microseconds.

         , cellSchedulePeriod :: Double
         -- ^The periodicity, in microseconds, at which the cell will
         -- be scheduled for 'runtime' microseconds.

         , cellSharedMemoryRegions :: [SharedMemoryAccess]
         -- ^The shared memory region access parameters.

         , cellArguments :: [String]
         -- ^Command-line arguments to be passed to the cell program.

         , cellExternalInterfaces :: [String]
         -- ^SELinux interfaces to be applied to the cell domain
         }
    deriving (Eq, Show)

data SharedMemoryRegion =
    SharedMemoryRegion { regionSize :: Integer
                       -- ^The size of the region, in bytes.

                       , regionName :: String
                       -- ^A name for the region.
                       }
    deriving (Eq, Show)

data SharedMemoryAccess =
    SharedMemoryAccess { accessType :: AccessType
                       -- ^How the shared memory region may be
                       -- accessed.

                       , accessRegionName :: String
                       -- ^The name of the region to be accessed.

                       , accessAlias :: String
                       -- ^An alias for the memory region with this
                       -- access type.

                       , accessMapAddress :: Maybe Integer
                       -- ^An address to map the memory to, if desired.
                       }
    deriving (Eq, Show)

data AccessType = MemReadOnly
                | MemWriteOnly
                | MemReadWrite
                  deriving (Eq, Show)

addCell :: Cell -> Layout -> Layout
addCell c l = l { layoutCells = layoutCells l ++ [c] }

chanBufSize :: Channel -> Integer
chanBufSize ch = chanMsgSlots ch * chanMsgSize ch

cellBandwidth :: Cell -> Double
cellBandwidth c = cellScheduleRuntime c / cellSchedulePeriod c
