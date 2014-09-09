module SCD.Lobster.Utilities
    ( splat       -- Split a list up at specified delimiter
    , scrunch     -- Inverse to splat
    , undot       -- splat specialsed to strings with dot as delimiter
    , redot       -- scrunch specialised to strings with dot as delimeter
    , capitalize  -- up the initial letter.
    ) where

import Data.List ( intercalate )
import Data.Char ( toUpper )

-- splatList will break up a list into a list of lists using a delimeter
-- character. Shame on the haskell library folks for not having this in
-- the library already.
-- Warning, the expression: acc ++ [x] is probably inefficient, another
-- formuation involving reverse will be desirable for applications that
-- accumulate long results.
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- specializes to foldl
-- foldl :: (([b], [[b]]) -> b -> ([b], [[b]])) -> ([b], [[b]]) -> [b] -> ([b], [[b]])
splat :: (Eq a) => a -> [a] -> [[a]]
splat delim = reverse . uncurry (:) . foldl f ([], [])
  where -- f :: (Eq a) => ( [a], [[a]] ) -> a -> ( [a], [[a]] )
        f ( acc, l ) x = if x == delim then ( [], acc:l ) else ( acc ++ [x], l )

-- Scrunch a list back together, using a specified delimiter
-- Should have: splat a (scrunch a as) = as
-- or just      splat a . scrunch a = id
-- and          splat a (scrunch a as) = as
-- or just      scrunch a . splat a = id
scrunch :: a -> [[a]] -> [a]
scrunch a as = intercalate [a] as

-- Specialize to strings with dot as delimiter
-- so: undot . redot = id
--     redot . undot = id
undot :: String -> [String]
undot = splat '.'

redot :: [String] -> String
redot = scrunch '.'

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize xs = xs


