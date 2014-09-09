module Sklite.Layout.Validation
    ( validateLayout
    , explodeChannels
    , metadataBufSize

    , slotsBufName
    , readerBufName
    , writerBufName

    , ValidatedLayout(..)
    , ExplodedLayout(..)
    )
where

import Control.Applicative ((<$>))
import Data.Char (isAlpha)
import Data.List (nub)

import Sklite.Types

newtype ValidatedLayout = ValidatedLayout Layout
    deriving (Show, Eq)

newtype ExplodedLayout = ExplodedLayout ValidatedLayout
    deriving (Show, Eq)

metadataBufSize :: Integer
metadataBufSize = 1024

validateCells :: Layout -> Either String ()
validateCells l = do
  let cs = layoutCells l
      theSum = sum $ cellBandwidth <$> cs
      msg = "Scheduling percentages of cells must not exceed " ++ (show $ layoutBandwidth l) ++ "%"
      validateRates = if (theSum * 100 > layoutBandwidth l) then
                          Left msg else
                          Right ()
      validateNames = if (length $ nub $ cellName <$> cs) /= (length cs)
                      then Left "All cell names must be unique"
                      else Right ()

      nonEmptyNames = if (any null $ cellName <$> cs)
                      then Left "Cell names must be non-empty"
                      else Right ()

      nonEmptyPrognames = if (any null $ cellProgram <$> cs)
                          then Left "Cell programs must be non-empty"
                          else Right ()

      nonEmptyAliases c = if (any null $ accessAlias <$> cellSharedMemoryRegions c)
                          then Left "Memory access aliases must be non-empty"
                          else Right ()

      uniqueAliases c = let as = cellSharedMemoryRegions c
                        in if (length $ nub $ accessAlias <$> as) /= (length as)
                           then Left "Memory access aliases must be unique to each cell"
                           else Right ()

      validAliases c = let formatCheck [] = False
                           formatCheck nam = isAlpha $ head nam
                           aliases = accessAlias <$> cellSharedMemoryRegions c
                       in if not $ null $ filter (not . formatCheck) aliases
                          then Left "Memory access aliases must start with a letter"
                          else Right ()

      regions = cellSharedMemoryRegions
      accessError c = concat [ "Cell " ++ (show $ cellName c)
                             , " contains multiple references to the same shared memory region"
                             ]
      numUniqueReferences c = length $ nub $ accessRegionName <$> regions c
      numAllReferences c = length $ regions c
      validateAccess c = if numUniqueReferences c /= numAllReferences c
                         then Left $ accessError c
                         else Right ()

      validateBw c = if cellScheduleRuntime c > cellSchedulePeriod c
                     then Left $ "Cell " ++ (show $ cellName c) ++ " has runtime larger than period"
                     else Right ()

  sequence_ $ validateBw <$> cs
  validateRates
  nonEmptyPrognames
  nonEmptyNames
  sequence_ $ nonEmptyAliases <$> cs
  sequence_ $ uniqueAliases <$> cs
  sequence_ $ validAliases <$> cs
  validateNames
  mapM_ validateAccess cs

validateChannels :: Layout -> Either String ()
validateChannels l = do
  let validChanFrom ch = if chanFrom ch `elem` (cellName <$> layoutCells l)
                         then Right ()
                         else Left $ "Invalid channel end: " ++ chanFrom ch

      validChanTo ch = if chanTo ch `elem` (cellName <$> layoutCells l)
                       then Right ()
                       else Left $ "Invalid channel end: " ++ chanTo ch

      validChanEnds ch = if chanFrom ch /= chanTo ch
                         then Right ()
                         else Left $ "Channel uses cell " ++ show (chanFrom ch) ++ " for both ends"

      names = chanName <$> layoutChannels l
      validateNames = if length names == (length $ nub names)
                      then Right ()
                      else Left "All channel names must be unique"

  mapM_ validChanFrom $ layoutChannels l
  mapM_ validChanTo $ layoutChannels l
  mapM_ validChanEnds $ layoutChannels l
  validateNames

slotsBufName :: Channel -> String
slotsBufName ch = "ch_" ++ chanName ch ++ "_slots"

readerBufName :: Channel -> String
readerBufName ch = "ch_" ++ chanName ch ++ "_reader"

writerBufName :: Channel -> String
writerBufName ch = "ch_" ++ chanName ch ++ "_writer"

channelMemRegions :: Channel -> [SharedMemoryRegion]
channelMemRegions chan =
    [ SharedMemoryRegion (chanBufSize chan) (slotsBufName chan)
    , SharedMemoryRegion metadataBufSize (readerBufName chan)
    , SharedMemoryRegion metadataBufSize (writerBufName chan)
    ]

getShmemAccess :: Cell -> Channel -> [SharedMemoryAccess]
getShmemAccess cell chan =
    getAccess
        where
          getAccess = getFromAccess ++ getToAccess

          getFromAccess =
              if cellName cell /= chanFrom chan
              then []
              else [ SharedMemoryAccess MemWriteOnly (slotsBufName chan) (slotsBufName chan) Nothing
                   , SharedMemoryAccess MemReadWrite (writerBufName chan) (writerBufName chan) Nothing
                   , SharedMemoryAccess MemReadOnly (readerBufName chan) (readerBufName chan) Nothing
                   ]

          getToAccess =
              if cellName cell /= chanTo chan
              then []
              else [ SharedMemoryAccess MemReadOnly (slotsBufName chan) (slotsBufName chan) Nothing
                   , SharedMemoryAccess MemReadOnly (writerBufName chan) (writerBufName chan) Nothing
                   , SharedMemoryAccess MemReadWrite (readerBufName chan) (readerBufName chan) Nothing
                   ]

explodeChannels :: ValidatedLayout -> Either String ExplodedLayout
explodeChannels (ValidatedLayout l) =
    ExplodedLayout <$> validateLayout final
        where
          final = addRegions $ updateCells l

          -- Add more shared memory regions to the existing list
          addRegions layout =
              let rs = sharedMemoryRegions layout
                  rest = concat $ channelMemRegions <$> layoutChannels layout
              in layout { sharedMemoryRegions = rs ++ rest }

          -- Replace cells with modified versions
          updateCells layout =
              let cs = layoutCells layout
              in layout { layoutCells = updateCell <$> cs }

          updateCell c =
              let as = cellSharedMemoryRegions c
                  rest = concat $ getShmemAccess c <$> layoutChannels l
              in c { cellSharedMemoryRegions = as ++ rest }

validateLayout :: Layout -> Either String ValidatedLayout
validateLayout layout = do
  -- Check that the shared memory regions referenced by the shells
  -- actually exist.
  let declaredRegions = regionName <$> sharedMemoryRegions layout
      accessRecords = concat $ cellSharedMemoryRegions <$> layoutCells layout
      references = accessRegionName <$> accessRecords
      invalid = filter (\n -> not $ n `elem` declaredRegions) references

      -- Check that the layout's required bandwidth does not exceed 100%.
      bwCheck = if layoutBandwidth layout > 100
                then Left "Layout bandwidth exceeds 100%"
                else Right ()

      shmemCheck = case invalid of
                     [] -> Right ()
                     is -> Left $ "No such shared memory regions: " ++ show is

  shmemCheck
  bwCheck
  validateCells layout
  validateChannels layout
  return $ ValidatedLayout layout
