module Sklite.ToolMain
    ( toolMain
    )
where

import Control.Monad (when)
import System.Exit
import System.Environment
import System.Console.GetOpt

import Sklite.Types
import Sklite.Layout.Validation
import Sklite.Config

data Option = Help
              deriving (Eq, Show)

options :: [OptDescr Option]
options = [ Option "h" ["help"] (NoArg Help)
            "This help output"
          ]

getLayout :: FilePath -> IO (Either String Layout)
getLayout = parseLayoutFile

usageHeader :: String -> String
usageHeader n =
    "Usage: " ++ n ++ " [options] <layout configuration path> <output directory>"

usage :: IO ()
usage = do
  n <- getProgName
  putStrLn $ usageInfo (usageHeader n) options

toolMain :: (FilePath -> ExplodedLayout -> IO ()) -> IO ()
toolMain act = do
  args <- getArgs
  let (opts, rest, _) = getOpt Permute options args

  when (Help `elem` opts) $ do
         usage
         exitSuccess

  (configPath, outputDir) <-
      case rest of
        [c, o] -> return (c, o)
        _ -> usage >> exitSuccess

  attempt <- getLayout configPath

  layout <- case attempt of
              Left e -> (putStrLn $ "Error in config file: " ++ e) >> exitFailure
              Right l -> return l

  validated <- case validateLayout layout of
                 Left e -> do
                   putStrLn $ "Error validating layout: " ++ e
                   exitFailure
                 Right validated ->
                     case explodeChannels validated of
                       Left e2 -> do
                         putStrLn $ "Error setting pu channels: " ++ e2
                         exitFailure
                       Right final -> return final

  act outputDir validated
