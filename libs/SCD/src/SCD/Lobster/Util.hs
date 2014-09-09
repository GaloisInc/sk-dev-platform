{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -cpp #-}
{- |
Module      : $Header$
Description : Working with Lobster abstract syntax.
Copyright   : (c) Galois, Inc.

Working with Lobster abstract syntax.
-}
module SCD.Lobster.Util where

import Lobster.Abs as L
import Text.Happy.ParserMonad(noPos)

import SCD.M4.Syntax as M4
import SCD.SELinux.Syntax ( IsIdentifier(..), mkId )
import SCD.Lobster.Utilities ( capitalize )

layerModule2Class :: LayerModule -> L.ClassId
layerModule2Class (l,m) = mkId (capitalize (idString l)++
                                capitalize (idString m))

m4Id2Class :: M4Id -> L.ClassId
m4Id2Class = mkId . capitalize . idString

m4Id2Identifier :: M4Id -> L.Identifier
m4Id2Identifier = mkId . idString

instance IsIdentifier L.ClassId where
  idString (L.ClassId i) = idString i
  mkId' p i = L.ClassId (mkId' p i)
  fromId i = L.ClassId (fromId i)
  toId (L.ClassId i) = toId i
  pos (L.ClassId i) = pos i

instance IsIdentifier L.PortId where
  idString (L.PortId i) = idString i
  mkId' p i = L.PortId (mkId' p i)
  fromId i = L.PortId (fromId i)
  toId (L.PortId i) = toId i
  pos (L.PortId i) = pos i

instance IsIdentifier L.Identifier where
  idString (L.Identifier i) = idString i
  mkId' p i = L.Identifier (mkId' p i)
  fromId i = L.Identifier (fromId i)
  toId (L.Identifier i) = toId i
  pos (L.Identifier i) = pos i

instance IsIdentifier L.UIdent where
  idString (L.UIdent i) = i
  mkId' _ i = L.UIdent i
  fromId i = L.UIdent (idString i)
  toId (L.UIdent i) = (mkId i)
  pos _ = noPos

instance IsIdentifier L.LIdent where
  idString (L.LIdent i) = i
  mkId' _ i = L.LIdent i
  fromId i = L.LIdent (idString i)
  toId (L.LIdent i) = (mkId i)
  pos _ = noPos

