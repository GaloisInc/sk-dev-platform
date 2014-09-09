{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  $Header$
Description :  Lobster abstract syntax
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Defines the abstract syntax for the Lobster high-level policy language.
-}
module Lobster.Syntax where

-- import Prelude
-- import qualified Data.List as List

import Lobster.Abs as Abs

--------------------------------------------------------------------------------
-- A class for identifiers. Identifiers can be converted back and forth from
-- strings, and back and forth from the type Identifier.
--------------------------------------------------------------------------------

class (Show a, Ord a) => IsIdentifier a where
  idString :: a -> String
  mkId :: String -> a
  fromId :: Identifier -> a
  toId :: a -> Identifier

instance IsIdentifier Identifier where
  idString (Identifier (LIdent s)) = s
  mkId = Identifier . LIdent
  fromId i = i
  toId i = i

instance IsIdentifier ClassId where
  idString (ClassId (UIdent s)) = s
  mkId = ClassId . UIdent
  fromId = mkId . idString
  toId = mkId . idString

instance IsIdentifier PortId where
  idString (PortId (LIdent s)) = s
  mkId = PortId . LIdent
  fromId = mkId . idString
  toId = mkId . idString

instance IsIdentifier FlowId where
  idString (FlowId (LIdent s)) = s
  mkId = FlowId . LIdent
  fromId = mkId . idString
  toId = mkId . idString

typeClass :: ClassId
typeClass = mkId "Type"

fileClass :: ClassId
fileClass = mkId "File"

dirClass :: ClassId
dirClass = mkId "Dir"

systemClass :: ClassId
systemClass = mkId "System"

typeFlow :: FlowId
typeFlow = mkId "type"

directionFlow :: FlowId
directionFlow = mkId "direction"

positionFlow :: FlowId
positionFlow = mkId "position"

--------------------------------------------------------------------------------
-- Port type constraints.
--------------------------------------------------------------------------------

toConstraintsPortDeclarationType :: PortDeclarationType -> [PortTypeConstraint]
toConstraintsPortDeclarationType pdt =
    case pdt of
      EmptyPDT -> []
      PortTypePDT ptcs -> ptcs

--------------------------------------------------------------------------------
-- The Policy data type is the top level of the AST, corresponding to the
-- Policy production in Parser.y.
--------------------------------------------------------------------------------

emptyPolicy :: Policy
emptyPolicy = Policy []

appendPolicy :: Policy -> Policy -> Policy
appendPolicy (Policy s1) (Policy s2) = Policy (s1 ++ s2)
