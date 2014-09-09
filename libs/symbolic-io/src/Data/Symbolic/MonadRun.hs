{-# OPTIONS_GHC -Wall -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Data.Symbolic.MonadRun
    ( MonadRun      ( .. )   -- Monad with a run operation
    , MonadRunTrans ( .. )   -- A monad transformer with a run operation
    , MonadRunSpecial ( .. ) -- A monad transformer with a special run operation
    ) where

import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Trans    ( MonadTrans )
import System.IO.Unsafe       ( unsafePerformIO )

class ( Monad m ) => MonadRun m where
   run :: m a -> a

class ( MonadTrans t ) => MonadRunTrans t where
   run1 :: ( Functor m, Monad m ) => t m a -> m a

class ( MonadTrans t ) => MonadRunSpecial t b | t -> b where
   run2 :: ( Functor m ) => t m a -> m b

instance MonadRun Identity where
   run = runIdentity

instance MonadRun IO where
   run = unsafePerformIO
