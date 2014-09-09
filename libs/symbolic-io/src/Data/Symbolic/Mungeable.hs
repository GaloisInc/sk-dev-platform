{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.Mungeable
    ( Mungeable ( .. ) -- A mungeable connection
    , Guzin ( .. )     -- Direction (into or out of) a mungeable connection
    ) where

import Data.Symbolic.Abs        ( ClassId )
import Data.Symbolic.LobsterAST ( LobsterAST )

data Guzin = Guzinta | Guzouta deriving ( Eq, Ord, Enum, Bounded, Show )

data Mungeable =
    Mungeable { cid           :: ClassId
              , direction     :: Guzin
              , specification :: LobsterAST
              } deriving ( Eq, Ord, Show )
