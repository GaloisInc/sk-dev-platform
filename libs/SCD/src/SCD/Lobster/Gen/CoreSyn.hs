{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  CSD.Lobster.Gen.CoreSyn
Description :  Simply representing Lobster.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

Stripped down representation of Lobster specs, for easier
generation and manipulation by programs. That's the intent,
anyway! :-)

-}
module SCD.Lobster.Gen.CoreSyn
       ( Module
       , Decl(..)
       , Name(..)
       , nameString
       , DomPort(..)
       , Dir(..)
       , Param
       , PortConstraint(..)

       , newClass
       , newType
       , newDomain
       , newPort
       , newComment

       , withConstraint
       , withConnection
       , dirPort

       , domPort
       , left
       , right
       , neutral
       , bidi
       , connect
       ) where

-- | @Module@ associates a name with Lobster declaration
-- list; the name could then be used to derive a filename/URL
-- for output..
type Module = (Name,[Decl])

data Decl
   -- I can't help it, but the use of 'statements' in Lobster
   -- seems odd. It's all about wiring things up, so
   -- an operational model or the notion of 'executing' the
   -- spec seems
 = Class   Name [Param] [Decl]
 | Port    Name [PortConstraint] (Dir,[DomPort])
 | Domain  Name Name [Param]
 | Type    Name [Name]
 | Connect DomPort DomPort Dir
 | Comment String

newtype Name = Name String
  deriving (Eq, Ord)

nameString :: Name -> String
nameString (Name s) = s

type PortDecl = Decl

newClass :: Name -> [Param] -> [Decl] -> Decl
newClass n ps body = Class n ps body

newType :: Name -> [Name] -> Decl
newType x as = Type x as

newComment :: String -> Decl
newComment = Comment

newPort :: Name -> PortDecl
newPort nm = Port nm [] (N,[])

withConstraint :: PortConstraint -> PortDecl -> PortDecl
withConstraint pc (Port nm cs ds) = Port nm (pc:cs) ds

withConnection :: DomPort -> Dir -> PortDecl -> PortDecl
withConnection dp d (Port nm cs (_,ds)) = Port nm cs (d,dp:ds)

dirPort :: Name -> Dir -> PortDecl
dirPort nm d = withConstraint (PortDir d) (newPort nm)

data DomPort
 = DomPort
     { portDomain :: Name
     , portLabel  :: Name
     }

data Dir = L | R | N | B

type Param = Name -- for now..

data PortConstraint
 = PortDir Dir
 | PortPos Bool  -- False => subject; True => ....yep,you guessed it..
 | PortType Name

newDomain :: Name -> Name -> [Param] -> Decl
newDomain binder ctor args = Domain binder ctor args

domPort :: Name -> Name -> DomPort
domPort a b = DomPort{portDomain=a,portLabel=b}

left :: DomPort -> DomPort -> Decl
left = connect L

right :: DomPort -> DomPort -> Decl
right = connect R

neutral :: DomPort -> DomPort -> Decl
neutral = connect N

bidi :: DomPort -> DomPort -> Decl
bidi = connect B

connect :: Dir -> DomPort -> DomPort -> Decl
connect d a b = Connect a b d

