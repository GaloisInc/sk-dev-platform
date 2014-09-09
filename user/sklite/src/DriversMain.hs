module Main where

import Sklite.ToolMain
import Sklite.Drivers

main :: IO ()
main = do
  toolMain $ \outputDir layout ->
      do
        generateDrivers outputDir layout
        putStrLn $ "Generated driver source in " ++ outputDir
