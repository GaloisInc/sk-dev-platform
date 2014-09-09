{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

module Main where

import System.Cmd         ( system )
import System.Environment ( getArgs )
import Control.Monad      ( when )

-- Ignoring the error returns for now
-- Top reads from standard input (in port of top) and writes to
-- standard output (out port of top)
-- Just do the writeFile from top, Lobster will take care of us

top :: IO ()
top =
  do { -- Following stuff has nothing to do with the Lobster use case
       -- It is for running and testing this program. We get names for
       -- the internal files of top
       args <- getArgs
     ; when ( length args /= 2 ) ( error ( "Arguments bad: " ++ show args ) )
       -- Now for the Lobster use case
       -- Set up the files inf and outf for the top domain
       -- | domain inf  = SimpleFile ( Topin, infName );   // Input file of top
       -- | in        --> inf.write; // Needs to be added externally
       -- | inf.read  --> p.active;
     ; infContents <- readFile ( args !! 0 )
     ; putStrLn ( "infContents = " ++ infContents ) -- Debugging
       -- Sneaky write into a.in, which is within a
       -- | p.active  --> a.in;
     ; writeFile "a.in" infContents
       -- | domain a = Inner ( "a.in", "a.out" );
     ; _e1 <- system ( "./A a.in a.out" )
       -- Similar sneaky read from a.out, which is within a
       -- | outf.read --> out;
       -- | p.active  --> outf.write;
     ; aoutContents <- readFile "a.out"
     ; putStrLn ( "aoutConents = " ++ aoutContents ) -- Debugging
       -- Sneaky write into b.in, which is within b
       -- | p.active --> b.in;
     ; writeFile "b.in" aoutContents
       -- |  domain b = Inner ( "b.in", "b.out" );
     ; _e2 <- system ( "./B b.in b.out" )
       -- Similar sneaky read from b.out, which is within b
       -- | b.out --> p.active
     ; boutContents <- readFile "b.out"
     ; putStrLn ( "boutConents = " ++ boutContents ) -- Debugging
       -- | outf.read --> out; // Needs to be added externally
       -- | p.active  --> outf.write;
     ; writeFile ( args !! 1 ) boutContents
     }

main :: IO ()
main = top
