{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror -cpp #-}

module Text.Happy.ParserMonad(P, parseError, mkParser, Pos(..), noPos)
  where

import Control.Monad.Error(ErrorT, runErrorT, MonadError(..))
import Control.Monad.Identity(Identity, runIdentity)
--import System.FilePath(FilePath)

newtype P a = P{ unP :: ErrorT String Identity a }
#ifndef __HADDOCK__
  deriving (Monad, MonadError String, Functor)
#endif

_P_not_used :: a
_P_not_used = undefined P

parseError :: Show t => [t] -> P a
parseError []     = throwError "Parse error at EOF"
parseError (t:_) = throwError $ "Parse error at " ++ show t

mkParser :: (String -> P a) -> String -> Either String a
mkParser p = runIdentity . runErrorT . unP . p

data Pos = Pos FilePath !Int !Int !Int
  deriving (Read, Show, Eq, Ord)

noPos :: Pos
noPos = Pos "" 0 0 0
