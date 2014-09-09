module Main where

import Support            ( inner )
import Control.Monad      ( when )
import System.Environment ( getArgs )

main :: IO ( )
main = 
  do { -- Following stuff has nothing to do with the Lobster use case
       -- It is for running and testing this program. We get names for
       -- the internal files of top
       args <- getArgs
     ; when ( length args /= 2 ) ( error ( "Arguments bad: " ++ show args ) )
       -- Now for the Lobster use case
     ; inner "B" ( args!!0 ) ( args!!1 )
     }
