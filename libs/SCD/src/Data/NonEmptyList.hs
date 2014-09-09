{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Data.NonEmptyList where

import Data.Foldable(Foldable(foldr), toList)
import Data.Traversable(Traversable(traverse))
import Data.Generics(Typeable, Data)
import Prelude hiding (reverse, foldr, head, tail)
import qualified Prelude

newtype NonEmptyList a = NE (a,[a])
  deriving (Typeable, Data, Eq, Ord, Show, Read)

head :: NonEmptyList a -> a
head (NE (a,_)) = a

tail :: NonEmptyList a -> [a]
tail (NE (_,l)) = l

instance Functor NonEmptyList where
  fmap f (NE (a,l)) = NE (f a,fmap f l)

instance Foldable NonEmptyList where
  foldr f z (NE (a,l)) = foldr f z (a:l)

instance Traversable NonEmptyList where
  traverse f = fmap fromList . traverse f . toList

class FromList l where
  fromList :: [a] -> l a 

instance FromList NonEmptyList where
  fromList [] = error "Data.NonEmptyList.fromList on empty list"
  fromList (a:l) = NE (a,l)

instance FromList [] where
  fromList = id

singleton :: a -> NonEmptyList a
singleton a = NE (a,[])

append :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
append a b = fromList (toList a++toList b)

cons :: a -> NonEmptyList a -> NonEmptyList a
cons a (NE (b,l)) = NE (a,b:l)

reverse :: NonEmptyList a -> NonEmptyList a
reverse (NE (a,l)) = NE (a',l') where (a':l') = Prelude.reverse (a:l)
