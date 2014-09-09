{-# OPTIONS_GHC -Wall -Werror -XScopedTypeVariables #-}
module SCD.Lobster.Symbolic.Connection
    ( GlobalConnection ( .. ) -- Information from both specs about a connection
    , LocalConnection ( .. )  -- Local information about a connection
    , PreConnection ( .. )    -- Preliminary information for a connection
    , mkLocal                 -- Constructor for LocalConnection
    , preToLocal              -- Convert PreConnection to LocalConnection
    , mkPre                   -- Constructor for PreConnection
    ) where

import Lobster.Abs ( Statement ( .. ), Identifier )
import Lobster.Print ( printTree )

data GlobalConnection
    = GlobalConnection { portConn  :: Statement  -- Port connection statement
                       , identFrom :: Identifier -- Identifier in one spec
                       , identTo   :: Identifier -- Identifier in other spec
                       , globName  :: String     -- File name of common file
                       , fromClass :: Identifier -- Class of reference
                       , toClass   :: Identifier -- Class of resource
                       } deriving ( Eq, Ord )

instance Show GlobalConnection where
    show ( GlobalConnection { portConn = s
                            , identFrom = f
                            , identTo = t
                            , globName = n
                            , fromClass = fi
                            , toClass = ti } ) =
        "{{ " ++ printTree s ++ ", " ++ printTree f ++
        ", " ++ printTree t ++ ", " ++ n ++ ", " ++ printTree fi ++
        ", " ++ printTree ti ++ " }}"

data LocalConnection
    = LocalConnection { localConn  :: Statement  -- For a port declaration
                      , localIdent :: Identifier -- For resource
                      , localName  :: String
                      , localId    :: Identifier -- For class referencing the resource
                      }

instance Show LocalConnection where
    show ( LocalConnection { localConn = s, localIdent = i, localName = n, localId = c } ) =
      "{ " ++ printTree s ++ ", " ++ printTree i ++
      ", " ++ n ++ ", " ++ printTree c ++ " }"

instance Eq LocalConnection where
    l1 == l2 = localName l1 == localName l2

data PreConnection = PreConnection { preIdent :: Identifier
                                   , preName  :: String
                                   }

instance Eq PreConnection where
    p1 == p2 = preName p1 == preName p2

instance Show PreConnection where
    show ( PreConnection { preIdent = i, preName = n } ) =
       "( " ++ printTree i ++ ", " ++ n ++ " )"

mkPre :: ( String, Identifier ) -> PreConnection
mkPre ( n, i ) = PreConnection { preIdent = i, preName = n }

mkLocal :: ( Statement, String, Identifier, Identifier ) -> LocalConnection
mkLocal ( statement, name, ident, from ) =
  LocalConnection { localConn = statement
                  , localIdent = ident
                  , localName = name
                  , localId = from
                  }

preToLocal :: Statement -> Identifier -> PreConnection -> LocalConnection
preToLocal statement from pre =
  LocalConnection { localConn  = statement
                  , localIdent = preIdent pre
                  , localName  = preName pre
                  , localId    = from
                  }


