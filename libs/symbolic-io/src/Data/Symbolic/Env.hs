{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.Env
    ( Env ( .. )        -- Environment for the SIO monad
    , Mode.Mode ( .. )  -- Re-export
    , emptyEnv          -- Make an empty environment
    , isGenerate        -- Determine if mode is Generate
    , isExecute         -- Determine if mode is Execute
    ) where

import qualified Data.Symbolic.Mode as Mode ( Mode ( .. ), isGenerate, isExecute )

data Env = Env { mode        :: Mode.Mode  -- Either Execute or Generate
               } deriving ( Eq, Show, Ord )

isGenerate :: Env -> Bool
isGenerate = Mode.isGenerate . mode

isExecute :: Env -> Bool
isExecute = Mode.isExecute . mode

emptyEnv :: Mode.Mode -> Env
emptyEnv m = Env { mode        = m
                 }
