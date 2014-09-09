{-# OPTIONS_GHC -Wall #-}
module Main where

import Support            ( startProcess )
import System.Environment ( getArgs )
import Control.Monad      ( when )

useCase1 :: IO ()
useCase1 =
  do { -- Following stuff has nothing to do with the Lobster use case
       -- It is for running and testing this program
       args <- getArgs
     ; when ( length args /= 2 ) ( error ( "Arguments bad: " ++ show args ) )
       ----------------------------------------------------------------------
       -- Now for the Lobster use case
       ----------------------------------------------------------------------
     ; startProcess "" "UseCase1" -- So we can generate domain for the process
       -- We are ingoring (for now) the possibility of IO errors
     ; c <- readFile ( args!!0 )
     ; writeFile ( args!!1 ) c
     ; putStrLn ( "Input was: " ++ c )
     }

main :: IO ()
main = useCase1
