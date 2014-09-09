
module Data.MapSet(union, difference, insert, delete, member, lookup,
  lookupWithKey, transitive, fixpoint) where

import Data.Map(Map, unionWith, differenceWith, insertWith,
  findWithDefault, mapWithKey)

import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)
import Data.Foldable(toList)

union :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
union = unionWith Set.union

difference :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
difference = differenceWith (\s1 s2 -> Just (Set.difference s1 s2))

insert :: (Ord a, Ord b) => a -> b -> Map a (Set b) -> Map a (Set b)
insert a b = insertWith Set.union a (Set.singleton b)

delete :: (Ord a, Ord b) => a -> b -> Map a (Set b) -> Map a (Set b)
delete a b m = if Set.null s' then Map.delete a m 
               else Map.insert a s' m
  where s' = Set.delete b (lookup a m)

member :: (Ord a, Ord b) => a -> b -> Map a (Set b) -> Bool
member a b = Set.member b . lookup a

lookup :: (Ord a, Ord b) => a -> Map a (Set b) -> Set b
lookup = findWithDefault Set.empty

lookupWithKey :: (Ord a, Ord b) => a -> Map a (Set b) -> Set (a,b)
lookupWithKey k = lookup k . mapWithKey (\a -> Set.map (\b -> (a,b)))

transitive :: (Eq a, Ord a) => Map a (Set a) -> Map a (Set a)
transitive = fixpoint transstep where
  transstep m = m `union` fmap (Set.unions . toList . Set.map (flip lookup m)) m

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = if a' == a then a else fixpoint f a'
  where a' = f a


