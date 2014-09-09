module Sklite.Boot
    ( generateBootScripts
    )
where

import Control.Applicative
import Data.List

import System.FilePath
import Sklite.Types
import Sklite.Layout.Validation
import qualified Sklite.Paths as Paths

crossBootScriptFilename :: FilePath
crossBootScriptFilename = "boot.sh"

hostBootScriptFilename :: FilePath
hostBootScriptFilename = "host-boot.sh"

generateBootScripts :: FilePath -> ExplodedLayout -> IO ()
generateBootScripts outputDir (ExplodedLayout (ValidatedLayout layout)) = do
  writeCrossBootScript (outputDir </> crossBootScriptFilename) layout
  writeHostBootScript (outputDir </> hostBootScriptFilename) layout

writeCrossBootScript :: FilePath -> Layout -> IO ()
writeCrossBootScript scriptPath layout =
    writeFile scriptPath $ bootScript True layout

writeHostBootScript :: FilePath -> Layout -> IO ()
writeHostBootScript scriptPath layout =
    writeFile scriptPath $ bootScript False layout

-- 1s
globalPeriod :: Double
globalPeriod = 1000000

bootScript :: Bool -> Layout -> String
bootScript doScheduling layout =
    unlines $ concat [ headerLines
                     , [""]
                     , startupLines
                     , [""]
                     , if doScheduling then schedulerLines else []
                     , [""]
                     , shmemLines
                     , [""]
                     , if doScheduling then ["restorecon -R /sk"] else []
                     , [""]
                     , launchLines
                     ]
    where
      logMsg = ("echo " ++) . show
      headerLines = [ "#!/bin/sh"
                    , "set -e"
                    , ""
                    ]

      startupLines = [ logMsg ">> Separation kernel development platform startup"
                     , logMsg $ "   Global bandwidth: " ++ (show $ layoutBandwidth layout) ++ "%"
                     ]

      schedulerLines = [ logMsg "Setting scheduling parameters:"
                       , logMsg "  Reducing root task group real-time bandwidth allocation"
                       , "echo 0 > /cgroup/cpu/cpu.rt_runtime_us"
                       , logMsg "  Reducing global real-time bandwidth allocation"
                       , "echo 0 > /proc/sys/kernel/sched_rt_runtime_us"
                       , logMsg "  Setting global deadline bandwidth allocation"
                       , "echo " ++ show ((truncate $ (layoutBandwidth layout / 100.0) * globalPeriod) :: Integer) ++
                                     " > /proc/sys/kernel/sched_dl_runtime_us"
                       , logMsg "  Setting global deadline period"
                       , "echo " ++ show ((truncate globalPeriod) :: Integer) ++
                                     " > /proc/sys/kernel/sched_dl_period_us"
                       ]

      shmemLines = concat $ shmemCommand <$> sharedMemoryRegions layout
      shmemCommand region =
          [ logMsg $ concat [ "Creating shared memory region "
                            , show $ regionName region
                            , " (" ++ (show $ regionSize region) ++ " bytes)"
                            ]
          , concat [ "dd if=/dev/zero of="
                   , Paths.regionFilename region
                   , " bs=1 count=" ++ (show $ regionSize region)
                   ]
          ]

      launchLines = concat $ launchCommand <$> layoutCells layout

      cellProgString cell =
          concat [ "./"
                 , intercalate " " $ cellProgram cell : cellArguments cell
                 ]

      launchCommand cell =
          [ logMsg $ "Starting cell: " ++ show (cellName cell)
          , logMsg $ "  Program:      " ++ (show $ cellProgram cell)
          , logMsg $ "  Runtime:      " ++ (show $ cellScheduleRuntime cell)
          , logMsg $ "   Period:      " ++ (show $ cellSchedulePeriod cell)
          , if doScheduling
            then concat [ "nohup schedtool -E -t "
                        , concat [ show ((truncate $ cellScheduleRuntime cell) :: Integer)
                                 , ":"
                                 , show ((truncate $ cellSchedulePeriod cell) :: Integer)
                                 , ":"
                                 , show ((truncate $ cellSchedulePeriod cell) :: Integer)
                                 ]
                        , " -e "
                        , cellProgString cell
                        , " > "
                        , cellName cell ++ ".log"
                        , " &"
                        ]
            else concat [ cellProgString cell
                        , " > "
                        , cellName cell ++ ".log"
                        , " &"
                        ]
          , ""
          ]

