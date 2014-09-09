
module Main where

import Sklite.ToolMain
import Sklite.SEPolicy

import System.FilePath

main :: IO ()
main = do
  toolMain $ \ outputDir layout -> do
    let output  = outputDir </> "sklite"
    writeFile (output ++ ".te")  $ generateTe layout
    writeFile (output ++ ".fc")  $ generateFc layout
    putStrLn $ "Generated policy in " ++ outputDir

