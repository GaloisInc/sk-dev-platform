module Main where

import qualified LayoutValidationTests
import qualified ConfigParsingTests
import qualified ChannelExplosionTests

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain [ LayoutValidationTests.tests
                   , ConfigParsingTests.tests
                   , ChannelExplosionTests.tests
                   ]