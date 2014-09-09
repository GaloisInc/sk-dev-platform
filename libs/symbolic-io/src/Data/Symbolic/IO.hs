{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -XFlexibleInstances -XMultiParamTypeClasses
                      -fno-warn-orphans -XTypeSynonymInstances #-}
{- |
Module      : $Header$
Description : Generation and execution of symbolic IO
Copyright   : (c) Galois, Inc.
License     : see the file LICENSE

Maintainer  : Peter White
Stability   : unstable
Portability : portable
 -}

module Data.Symbolic.IO
    ( -- * Types and invocation
      SIO                -- Symbolic IO monad
    , runExecute         -- Run an SIO action in execute mode
    , runGenerate        -- Run an SIO action in generate mode
    , sio                -- Constructor for SIO
    , isExecute          -- Are we executing or generating trace?
      -- * Control operations
    , cases
    , listCases
      -- * Lifted versions of corresponding operations in "Control.Monad"
    , when               -- Conditional execution in a monad
    , unless             -- Conditional execution in a monad
    , forever            -- Loop in a monad
    , runStatic
    ) where

import Prelude hiding         ( readFile, writeFile, putStrLn )
import Control.Monad.Reader   ( asks, local )
import Data.Symbolic.SIO      ( SIO, runSIO, when, unless, forever, sio )
import qualified Data.Symbolic.Env as Env
                              ( Mode ( .. ), isExecute, emptyEnv )
import Data.Monoid            ( Monoid )

-- Actually execute the actions
runExecute :: ( Functor m ) => SIO t m a -> m a
runExecute = fmap fst . runSIO ( Env.emptyEnv Env.Execute )

-- Just generate a trace of what actions would have been run.
runGenerate :: ( Functor m ) => SIO t m a -> m t
runGenerate = fmap snd . runSIO ( Env.emptyEnv Env.Generate )

isExecute :: (Monad m, Monoid t) => SIO t m Bool
isExecute = asks Env.isExecute

-- Perform case analysis on value that is only defined in execution
-- mode.  If in execution mode, the concrete value of type @a@ is
-- applied to the function, otherwise, it's undefined, so we apply the
-- function to all possible values to get all cases.
cases :: ( Enum a, Bounded a, Monad m, Monoid t) => a -> (a -> SIO t m b) -> SIO t m b
cases = listCases [minBound .. maxBound]

-- Perform case analysis on value that is only defined in execution
-- mode. If in execution mode, the concrete value of type @a@ is
-- applied to the function, otherwise, it's undefined, so we apply the
-- function to the provided list of values to get all cases.
listCases :: ( Monad m, Monoid t ) => [a] -> a -> (a -> SIO t m b) -> SIO t m b
listCases l a m
  = do { e <- asks Env.isExecute
       ; if e || null l then m a else head `fmap` mapM m l
       }

-- Execute the operation at trace-generation time.
runStatic :: ( Monad m, Monoid t ) => SIO t m a -> SIO t m a
runStatic = local ( const ( Env.emptyEnv Env.Execute ) )
