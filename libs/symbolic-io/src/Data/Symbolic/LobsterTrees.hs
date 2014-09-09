{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.LobsterTrees ( main ) where

import Data.Symbolic.SIO         ( SIO )
import Data.Symbolic.Abs         ( Policy ( .. ) )
import Prelude hiding            ( readFile, putStrLn, writeFile )
import qualified Prelude
import Data.Symbolic.LobsterAST  ( LobsterAST (..) )
import Data.Tree                 ( Tree, unfoldTree )
import System.IO.Unsafe          ( unsafePerformIO )
import Data.Symbolic.Lobster     ( generateProcess, readFile, writeFile
                                 , putStrLn )

useCase0 :: FilePath -> FilePath -> SIO LobsterAST IO ()
useCase0 _inf _outf = putStrLn ( "Hello World" )

processA :: FilePath -> FilePath -> SIO LobsterAST IO ()
processA inf outf =
  do { putStrLn ( ">>> starting process: A" )
       -- We are ingoring (for now) the possibility of IO errors
       -- | port in  : { position = object };
       -- | domain inf  = SimpleFile ( X, infName );
     ; fileContents <- readFile inf
       -- | domain outf = SimpleFile ( Y, outfName );
       -- | port out : { position = object };
     ; writeFile outf fileContents
     }

processB :: FilePath -> FilePath -> SIO LobsterAST IO ()
processB inf outf =
  do { putStrLn ( ">>> starting process: B" )
       -- We are ingoring (for now) the possibility of IO errors
       -- | port in  : { position = object };
       -- | domain inf  = SimpleFile ( X, infName );
     ; fileContents <- readFile inf
       -- | domain outf = SimpleFile ( Y, outfName );
       -- | port out : { position = object };
     ; writeFile outf fileContents
     }

--------------------------------------------------------------------------------
-- Bundle up as test cases for convenience of the tester
--------------------------------------------------------------------------------

t :: IO ()
t = do { Prelude.putStrLn ( "astA:\n" ++ show astA )
       ; Prelude.putStrLn ( "astB:\n" ++ show astB )
       ; Prelude.putStrLn ( "astTop:\n" ++ show astTop )
       ; Prelude.putStrLn ( "tree:\n" ++ show tree )
       }

main :: IO ()
main = t

--------------------------------------------------------------------------------
-- Experiment with trees
--------------------------------------------------------------------------------

astA :: LobsterAST
astA = unsafePerformIO ( generateProcess "A" ( processA "a.in" "a.out" ) )

astB :: LobsterAST
astB = unsafePerformIO ( generateProcess "B" ( processB "b.in" "b.out" ) )

astTop :: LobsterAST
astTop = unsafePerformIO ( generateProcess "top" ( useCase0 "in" "out" ) )

useCase2Tree :: String -> ( ( String, Policy ), [ String ] )
--useCase2Tree "top" = ( policy astTop, [ "A", "B" ] )
useCase2Tree "top" = ( ( "top", Policy [] ), [ "A", "B" ] )
-- useCase2Tree "A" = ( policy astA, [ ] )
useCase2Tree "A" = ( ( "A", Policy [] ), [ ] )
-- useCase2Tree "B" = ( policy astB, [ ] )
useCase2Tree "B" = ( ( "B", Policy [] ), [ ] )
useCase2Tree _ = error ( "useCase2Tree" )

tree :: Tree ( String, Policy )
tree = unfoldTree useCase2Tree "top"
