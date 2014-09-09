module Main where

import Sklite.ToolMain
import Sklite.Boot

main :: IO ()
main = do
  toolMain $ \outputDir layout ->
      do
        generateBootScripts outputDir layout
        putStrLn $ "Generated boot scripts in " ++ outputDir
