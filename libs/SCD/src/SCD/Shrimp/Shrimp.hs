{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror -XCPP #-}
{- |
Module      :  SCD.Shrimp.Shrimp
Description :  Shrimp abstract syntax
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

The abstract syntax for Shrimp is collected in this module,
building on the abstract syntax for SELinux policies.

-}
module SCD.Shrimp.Shrimp
       ( Domain(..)
       , Role(..)
       , Template
       , runTemplate
       , Interface(..)
       , interface
       , newType
       , Type(..)
       , IsType(..)
       , Rule(..)
       ) where

import SCD.SELinux.Syntax(Stmt,IsIdentifier, TypeId, RoleId, mkId)

import Control.Monad.State.Class(MonadState, get, put)
import Control.Monad.Writer.Class(MonadWriter, tell)
import Control.Monad.Identity(Identity, runIdentity)
import Control.Monad.State.Lazy(StateT, runStateT)
import Control.Monad.Writer.Lazy(WriterT, runWriterT)


import Data.Set(Set,member, insert, empty)
import Data.Monoid(Monoid)
import Data.List(intersperse)

---

-- | At the base of SELinux and Shrimp, we've got @Type@;
-- unique labels covering some attribute of objects in the
-- SELinux universe...
newtype Type = Type { typeId :: TypeId }
  deriving (Eq, Read, Show, Ord, IsIdentifier)


-- | Shrimp @Rule@s are either SELinux policy statements or
-- role declarations, associating a 'Role' with a 'Domain'.
data Rule = 
   StmtRule Stmt
 | RoleRule Role Domain
  deriving (Eq, Read, Show)


-- | A @Domain@ is a non-persistent 'Type', and a component
-- of the security context.
newtype Domain = Domain { domainType :: Type }
  deriving (Eq, Read, Show, Ord, IsIdentifier, IsType)


-- | An @Interface@ is made up of a collection of 'Rule's.
newtype Interface = Interface{ unInterface :: [Rule] }

-- | A Shrimp @Role@ makes up the role part of the security context
-- of an active process.
newtype Role = Role RoleId
  deriving (Eq, Read, Show, Ord, IsIdentifier)

-- | A Shrimp interface @Template@ is an action that when performed will
-- produce a set of rules representing the interface's rules along with
-- the exported types of the interface. In addition to the value of
-- the action itself, of course.
newtype Template a = Template (StateT State (WriterT Output Identity) a)
#ifndef __HADDOCK__
  deriving (Monad, Functor, MonadState State, MonadWriter Output)
#endif

-- | To add a new type to a 'Template' definition, @newType comps@ is
-- used, delivering the type as result along with including it as part
-- of the template's interface.
newType :: IsType t => [String] -> Template t
newType l = do
   st <- get
   let s i = Type $ mkId $ concat $ intersperse "_" (l ++ if i == 0 then ["t"] else [show i,"t"])
       ids = map s [(0::Integer)..]
       ts = types st
       t = head $ filter (not . (`member`ts)) ids
   put st{ types = insert t ts }
   return (fromType t)


-- | Given a process Domain @d@, @interface d iFuns@ applies it
-- to each of the 'Domain'-parameterized interfaces in @iFuns@,
-- producing a composite @Template@ as a result, i.e., the roles
-- that make up each interface is included in the resulting template.
-- 
interface :: Domain -> [Domain -> Interface] -> Template ()
interface d = mapM_ (tell . Output . unInterface . ($d))

-- | To execute/instantiate a 'Template', wrap it up with @runTemplate@
-- It returns the triple @(v,rules,types)@.
runTemplate :: Template a -> (a,[Rule],Set Type)
runTemplate (Template m) = (a,deOutput w, types s)
   where ((a,s),w) = runIdentity $ runWriterT $ runStateT m State{ types = empty }

-- Supporting classes and definitions; monad machinery first:

newtype  State = State{ types :: Set Type }

newtype Output = Output { deOutput :: [Rule] }
  deriving (Eq, Read, Show, Monoid)

class IsIdentifier t => IsType t where
  toType   :: t -> Type
  fromType :: Type -> t

instance IsType Type where
  toType = id
  fromType = id

