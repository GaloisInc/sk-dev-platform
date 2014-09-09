{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -XFlexibleContexts #-}
module Data.Symbolic.Sample
    ( -- Some operations in the SIO monad
      readFile    -- Read string from specified file path
    , writeFile   -- Write string to specified filel path
    , putStrLn    -- Write a string to standard output
    ) where

import Data.Symbolic.Env             ( Env ( .. ) )
import Data.Symbolic.Trace           ( Step ( .. ), Trace )
import Prelude hiding ( readFile, writeFile, putStrLn )
import qualified Prelude ( readFile, writeFile, putStrLn )
import Data.Symbolic.SIO             ( SIOT, sio )

readFile :: FilePath -> SIOT Env Trace IO String
readFile f = sio ( Prelude.readFile f, [ ReadFile f ] )

writeFile :: FilePath -> String -> SIOT Env Trace IO ()
writeFile f s = sio ( Prelude.writeFile f s, [ WriteFile f s ] )

putStrLn :: String -> SIOT Env Trace IO ()
putStrLn s = sio ( Prelude.putStrLn s, [ WriteFile "/dev/stdout" s ] )
