module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.API ( Test )

import Test.HUnit hiding ( Test )

import Lobster.Symbion
import Policy

import qualified Symbion

main :: IO ()
main = do lobsterPolicies <- Policy.getLobsterPolicies
          defaultMain $ tests lobsterPolicies



tests lobsterPolicies = [ testGroup "Policy Tests" $ Policy.testCases lobsterPolicies
                        , testGroup "Symbion Tests" Symbion.testCases ]



