{-# OPTIONS_GHC -Wall #-}
module SCD.Lobster.Symbolic.LobsterUseCase ( main, pae, pbe, t0e, t1e, t2e ) where

import Prelude hiding                   ( readFile, putStrLn, writeFile )
import qualified Prelude
import SCD.Lobster.Symbolic.LobsterAST  ( LobsterAST (..), getClassStatements
                                        , getClassName, getLobsterClassId )
import SCD.Lobster.Symbolic.Lobster ( M, runExecute, runGenerate, readFile, writeFile
                                    , putStrLn, system )
import SCD.Lobster.Symbolic.Munge   ( whatFiles, munge, whatProcesses )
import SCD.Lobster.Syntax.Print     ( printTree )

--------------------------------------------------------------------------------
-- Use case 1, in SIO style, corresponds to UseCase1.hs
--------------------------------------------------------------------------------

useCase0 :: FilePath -> FilePath -> M ()
useCase0 _inf _outf = putStrLn ( "Hello World" )

useCase1 :: FilePath -> FilePath -> M ()
useCase1 inf outf = do { c <- readFile inf
                       ; writeFile outf c
                       ; putStrLn ("Input was: " ++ c )
                       }

useCase2 :: FilePath -> FilePath -> M ()
useCase2 inf outf =
  do { -- Set up the files inf and outf for the top domain
       -- | domain inf  = SimpleFile ( Topin, infName );   // Input file of top
       -- | in        --> inf.write; // Needs to be added externally
       -- | inf.read  --> p.active;
     ; infContents <- readFile inf
     ; putStrLn ( "infContents = " ++ infContents ) -- Debugging
       -- Sneaky write into a.in, which is within a
       -- | p.active  --> a.in;
     ; writeFile "a.in" infContents
       -- | domain a = Inner ( "a.in", "a.out" );
     ; _e1 <- system ( "A a.in a.out" )
       -- Similar sneaky read from a.out, which is within a
       -- | outf.read --> out;
       -- | p.active  --> outf.write;
     ; aoutContents <- readFile "a.out"
     ; putStrLn ( "aoutConents = " ++ aoutContents ) -- Debugging
       -- Sneaky write into b.in, which is within b
       -- | p.active --> b.in;
     ; writeFile "b.in" aoutContents
       -- |  domain b = Inner ( "b.in", "b.out" );
     ; _e2 <- system ( "B b.in b.out" )
       -- Similar sneaky read from b.out, which is within b
       -- | b.out --> p.active
     ; boutContents <- readFile "b.out"
     ; putStrLn ( "boutConents = " ++ boutContents ) -- Debugging
       -- | outf.read --> out; // Needs to be added externally
       -- | p.active  --> outf.write;
     ; writeFile outf boutContents
     }

processA :: FilePath -> FilePath -> M ()
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

processB :: FilePath -> FilePath -> M ()
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

t0e :: IO ()
t0e = runExecute ( useCase0 "Data/Symbolic/UseCase1.in" "Data/Symbolic/UseCase1.out" )

t0g :: IO LobsterAST
t0g = runGenerate "useCase0" ( useCase0 "in" "out" )

t1e :: IO ()
t1e = runExecute ( useCase1 "Data/Symbolic/UseCase1.in" "Data/Symbolic/UseCase1.out" )

t1g :: IO LobsterAST
t1g = runGenerate "useCase1" ( useCase1 "Data/Symbolic/UseCase1.in" "Data/Symbolic/UseCase1.out" )

t2e :: IO ()
t2e = runExecute ( useCase2 "Data/Symbolic/UseCase2.in" "Data/Symbolic/UseCase2.out" )

t2g :: IO LobsterAST
t2g = runGenerate "top" ( useCase2 "top.in" "top.out" )

pae :: IO ()
pae = runExecute ( processA "a.in" "a.out" )

pag :: IO LobsterAST
pag = runGenerate "A" ( processA "a.in" "a.out" )

pbe :: IO ()
pbe = runExecute ( processB "b.in" "b.out" )

pbg :: IO LobsterAST
pbg = runGenerate "B" ( processB "b.in" "b.out" )

t :: IO ()
t = do { t0gres <- t0g
       ; Prelude.putStrLn ( "t0g:\n" ++ ( printTree ( policy t0gres ) ) )
       ; t1gres <- t1g
       ; Prelude.putStrLn ( "class name for t1 = " ++ getClassName t1gres )
       ; Prelude.putStrLn ( "t1g:\n" ++ printTree ( policy t1gres ) )
--        ; Prelude.putStrLn ( "munge t1g:\n" ++ ( showLobsterAST mungedt1 ) )
--        ; if t1gres /= mungedt1
--          then Prelude.putStrLn "***** munge of t1 was different"
--          else Prelude.putStrLn "***** munge of t1 was the same" 
       ; t2gres1 <- t2g
       ; let t2gres = t2gres1 { moniker = "top" } -- Hack ***** possible fix needed in runGenerate
       ; Prelude.putStrLn ( "t2g:\n" ++ printTree ( policy t2gres ) )
       ; pagres1 <- pag
       ; let pagres = pagres1 { moniker = "a" } -- Hack *****
       ; Prelude.putStrLn ( "pag(" ++ getClassName pagres ++
                            ", " ++ show ( length ( getClassStatements pagres ) ) ++
                            ")\n" ++ printTree ( policy pagres ) )
       ; Prelude.putStrLn ( "Files in pag: " ++ show ( whatFiles pagres ) )
       ; pbgres1 <- pbg
       ; let pbgres = pbgres1 { moniker = "b" } -- Hack *****
       ; Prelude.putStrLn ( "pbg:\n" ++ printTree ( policy pbgres ) )
       ; Prelude.putStrLn ( "Files in pbg: " ++ show ( whatFiles pbgres ) )
         -- Tree for use case 2
--        ; let sub1 = Node { val = pagres, forest = [ ] }
--              sub2 = Node { val = pbgres, forest = [ ] }
--              uc2tree = Node { val = t2gres, forest = [ sub1, sub2 ] }
--              uc2mungedTree = munge uc2tree
--              mungedt2 = val uc2mungedTree
       ; let ss1 = getClassStatements t2gres
       ; Prelude.putStrLn ( "t2gres size = " ++ show ( length ss1 ) )
       ; Prelude.putStrLn ( "t2gres classId = " ++ show ( getLobsterClassId t2gres ) )
       ; Prelude.putStrLn ( "whatFiles t2gres = " ++ show ( whatFiles t2gres ) )
       ; Prelude.putStrLn ( "whatFiles pagres = " ++ show ( whatFiles pagres ) )
       ; Prelude.putStrLn ( "\n" ++ replicate 80 '-' ++ "\n" )
       ; let ( t2gres', pagres' ) = munge t2gres pagres
       ; Prelude.putStrLn ( "whatProcesses t2gres = " ++ show ( whatProcesses t2gres ) )
       ; Prelude.putStrLn ( "whatProcesses pagres = " ++ show ( whatProcesses pagres ) )
       ; Prelude.putStrLn ( "munged t2g:\n" ++ ( printTree ( policy t2gres' ) ) )
       ; Prelude.putStrLn ( "\n" ++ replicate 80 '-' ++ "\n" )
       ; Prelude.putStrLn ( "munged pagres:\n" ++ ( printTree ( policy pagres' ) ) )
--       ; Prelude.putStrLn ( "munge t2g:\n" ++ ( showLobsterAST mungedt2 ) )
--       ; if t2gres /= mungedt1
--         then Prelude.putStrLn "***** munge of t2 was different"
--         else Prelude.putStrLn "***** munge of t2 was the same"
       ; Prelude.putStrLn ( "All done" )
       }

main :: IO ()
main = t
