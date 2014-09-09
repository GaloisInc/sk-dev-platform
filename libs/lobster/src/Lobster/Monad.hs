{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -cpp #-}
{- |
Module      :  $Header$
Description :  A custom error monad
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

A custom error monad for SELinux modules.
-}
module Lobster.Monad(
  P,
  runP,
  canRunP,
  throwError,
  catchError,
  wrapError) where

import qualified Control.Monad.Error as Error(ErrorT(..))
import qualified Control.Monad.Error.Class as ErrorClass(MonadError(..))
import qualified Control.Monad.Identity as Identity(Identity(..))
import Control.Applicative(Applicative(..))
import Control.Monad(ap)

newtype P a = P { unP :: Error.ErrorT String Identity.Identity a }
    deriving (Monad,
#ifndef __HADDOCK__
              ErrorClass.MonadError String,
#endif
              Functor)

_GHC_WORKAROUND_P :: ()
_GHC_WORKAROUND_P = undefined P

instance Applicative P where
  pure = return
  (<*>) = ap

runP :: P a -> Either String a
runP = Identity.runIdentity . Error.runErrorT . unP

canRunP :: P a -> Bool
canRunP x =
    case runP x of
      Left _ -> False
      Right _ -> True

instance Eq a => Eq (P a) where
    x == y = runP x == runP y

throwError :: String -> P a
throwError e = ErrorClass.throwError e

catchError :: P a -> (String -> P a) -> P a
catchError p f =
    case runP p of
      Left e -> f e
      Right x -> return x

wrapError :: P a -> String -> P a
wrapError p s = p `catchError` (\e -> throwError $ s ++ ":\n" ++ e)
