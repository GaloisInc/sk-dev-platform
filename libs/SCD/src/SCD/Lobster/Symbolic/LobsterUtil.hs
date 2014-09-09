{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module SCD.Lobster.Symbolic.LobsterUtil
    ( active    -- Make qualified name for an active port
    , child     -- Make qualified name for a child port
    , getattr   -- Make qualified name for a getatter port
    , readPort  -- Make qualified name for a read port
    , writePort -- Make qualified name for a write port
    , createPort -- Make qualified name for a create port
    ) where

import SCD.Lobster.Symbolic.AstUtil    ( QualifiedName )

active :: String -> QualifiedName
active p = ( p, "active" )

child :: String -> QualifiedName
child p = ( p, "child" )

getattr :: String -> QualifiedName
getattr f = ( f, "getattr" )

readPort :: String -> QualifiedName
readPort f = ( f, "read" )

writePort :: String -> QualifiedName
writePort f = ( f, "write" )

createPort :: String -> QualifiedName
createPort f = ( f, "create" )

