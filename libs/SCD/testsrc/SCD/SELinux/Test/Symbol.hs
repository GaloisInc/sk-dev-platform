{- |
Module      :  $Header$
Description :  Tests the SELinux policy symbol table
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Tests the SELinux policy symbol table.
-}
module SCD.SELinux.Test.Symbol
    ( testCases, tryBuildSymbolTable
    )
where

import SCD.SELinux.Syntax       ( Policy )
import SCD.SELinux.Symbol       ( SymbolTable, build, summarize )
import SCD.SELinux.Test.Parser  ( parsePolicyFile )

import Test.Framework.Providers.API   ( Test )
import Test.Framework.Providers.HUnit ( testCase )

tryBuildSymbolTable :: Policy -> Either String (Policy,SymbolTable)
tryBuildSymbolTable policy = build policy

-- buildSymbolTable :: String -> Policy -> (Policy,SymbolTable)
-- buildSymbolTable filename policy =
--     case tryBuildSymbolTable policy of
--       Left err ->
--           error ("ERROR: couldn't build the symbol table for " ++ filename ++
--                  ":\n" ++ err)
--       Right policy_symbols ->
--           policy_symbols

-- buildSymbolTableTest :: String -> Policy -> IO ()
-- buildSymbolTableTest filename policy =
--     do (policy',symbols) <- return (buildSymbolTable filename policy)
--        putStr ("SUCCESS: built the symbol table for " ++ filename ++
--                ":\n" ++ summarize symbols)
--        return ()

symbolFileTest :: String -> IO ()
symbolFileTest filename =
    do policy <- parsePolicyFile filename
       case tryBuildSymbolTable policy of
         Left err -> error ("ERROR: couldn't build the symbol table for "++
                            filename ++":\n" ++ err)
         Right _  -> return ()

symbolFileExample :: FilePath
symbolFileExample = "testsrc/SCD/SELinux/Test/data/example.conf"

testCases :: FilePath -> [Test]
testCases filename = [ testCase ("Building symbol table for "++filename) $
                                symbolFileTest filename
                     ]

-- checks :: String -> Policy -> IO (Policy,SymbolTable)
-- checks filename policy = do
--       putStrLn "\nBegin tests of the SELinux policy symbol table"
--       _ <- symbolFileTest "src/SCD/SELinux/Test/data/example.conf"
--       (policy',symbols) <- buildSymbolTableTest filename policy
--       putStrLn "End tests of the SELinux policy symbol table"
--       return (policy',symbols)
