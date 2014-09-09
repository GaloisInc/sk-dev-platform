{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
module Data.List.GroupSort(groupSort, sortProj, groupProj) where

import Data.List(groupBy, sortBy)
import Data.Function(on)

groupSort :: Ord b => (a -> b) -> [a] -> [(b,[a])]
groupSort f ls = [(f (head l), l) | l <- groupBy ((==) `on` f) (sortProj f ls)]

groupProj :: Eq b => (a -> b) -> [a] -> [[a]]
groupProj f = groupBy ((==) `on` f)

sortProj :: Ord b => (a -> b) -> [a] -> [a]
sortProj f = sortBy (compare `on` f)
