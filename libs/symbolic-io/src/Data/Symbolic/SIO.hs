{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -XFlexibleContexts #-}
module Data.Symbolic.SIO
    ( SIO ( .. )        -- Reader / Writer monad transformer
    , lift              -- Lift into SIO
    , runSIO            -- Run an action in SIOT specialized to IO monad
      -- Constructor(s) of SIO actions
    , sio               -- The official create a trace / execution action
    , sio'              -- Same thing, but listifies the second (trace) argument
      -- Lifted versions of corresponding operations in "Control.Monad"
    , when              -- Like the when in Control.Monad
    , forever           -- Like the forever in Control.Monad
    , unless            -- Like the unless in Control.Monad
    ) where

import Control.Monad.Reader               ( ReaderT, runReaderT, MonadReader, ask, asks )
import Control.Monad.Writer               ( WriterT, runWriterT, MonadWriter, tell )
import Control.Monad.Trans                ( MonadTrans, lift )
import Data.Monoid                        ( Monoid )
import qualified Data.Symbolic.Env as Env ( Env ( .. ), isGenerate, isExecute )
import qualified Control.Monad as CM      ( when, forever, unless )

newtype SIO t m a = SIO ( ReaderT Env.Env (WriterT t m) a)
    deriving ( Functor, Monad, MonadReader Env.Env, MonadWriter t )

instance ( Monoid t ) => MonadTrans ( SIO t ) where
    lift m = SIO ( lift ( lift m ) )

runSIO :: Env.Env -> SIO t m a -> m (a, t)
runSIO r (SIO m) = runWriterT (runReaderT m r)

-- First version of sio, to produce a program that can be executed,
-- or can be used to generate the SELinux policy for the program.
sio :: ( Monad m, Monoid t ) => ( m a, t ) -> SIO t m a
sio ( m,  t ) =
  let err = error "Data.Symbolic.SIO: IO result demanded in non-execute mode"
  in do { r <- ask
        ; when ( Env.isGenerate r ) ( tell t )
        ; lift ( if Env.isExecute r then m else return err )
        }

-- A way to reduce typing a little bit. Not used right now
sio' :: ( Monad m ) => ( m a, t ) -> SIO [t] m a
sio' ( m,  t ) = sio ( m,  [t] )

--------------------------------------------------------------------------------
-- Some analogues of the Control.Monad operations
--------------------------------------------------------------------------------
when :: ( Monad m, Monoid t ) => Bool -> SIO t m () -> SIO t m ()
when b m = ifExecute (CM.when b m) m

forever :: ( Monad m, Monoid t ) => SIO t m a -> SIO t m ()
forever m = ifExecute (CM.forever m) (m >> return ())

unless :: ( Monad m, Monoid t ) => Bool -> SIO t m () -> SIO t m ()
unless b m = ifExecute (CM.unless b m) m

------------------------------------------------------------------------------------------
-- Internal stuff
------------------------------------------------------------------------------------------

ifExecute :: ( Monad m, Monoid t ) =>
    SIO t m a -> SIO t m a -> SIO t m a
ifExecute mt me = do { e <- asks Env.isExecute
                     ; if e then mt else me
                     }

