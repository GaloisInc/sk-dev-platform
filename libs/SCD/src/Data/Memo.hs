{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators #-}
module Data.Memo(
   Memo(..)
 , memoArray
 , toWordList
 , fromWordList
 , memoWordList
 , fib
 , fib'
 ) where

#ifndef __HADDOCK__
import GHC.Base(Unit(..), (:*:)(..), (:+:)(..))
#endif

import Data.Array(Ix, (!), array)
import Data.Word(Word8)

class Memo a where
  memo :: (a -> b) -> a -> b
#ifndef __HADDOCK__
  memo {| Unit |}    f = \_ -> fUnit where fUnit = f Unit
  memo {| a :*: b |} f = \(x :*: y) -> fxy x y
                       where fxy = memo $ \x' -> memo $ \y' -> f (x' :*: y')
  memo {| a :+: b |} f = \a -> case a of Inl a -> fInl a
                                         Inr b -> fInr b
                       where fInl = memo (f . Inl)
                             fInr = memo (f . Inr)
#endif

instance Memo ()
instance Memo Bool
instance Memo a => Memo (Maybe a)
instance Memo a => Memo [a]
instance (Memo a, Memo b) => Memo (a,b)

memoArray :: (Enum a, Bounded a, Ix a) => (a -> b) -> (a -> b)
memoArray f = (a!)
  where  a = array (minBound,maxBound) [(x,f x) | x <- [minBound..maxBound]]

toWordList :: forall a b. (Integral a, Integral b) => Int -> a -> [b]
toWordList _ 0 = []
toWordList bound a = fromIntegral r : toWordList bound q
   where (q,r) = quotRem a (fromIntegral bound)

fromWordList :: forall a b. (Num a, Integral b) => Int -> [b] -> a
fromWordList bound = foldr (\b a->a*(fromIntegral bound)+fromIntegral b) 0

memoWordList :: forall a b. Integral a => Int -> (a -> b) -> (a -> b)
memoWordList bound f = memo (f . fromWordList bound) . (toWordList bound :: a -> [Word8])

instance Memo Word8 where memo = memoArray

chunkSize :: Int
chunkSize = 256

instance Memo Integer where memo = memoWordList chunkSize
instance Memo Int where memo = memoWordList chunkSize

instance Memo Char where memo f = memo (f . toEnum) . fromEnum


-- example

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2) + 1

fib' :: Integer -> Integer
fib' = memo fib''
  where fib'' 1 = 1
        fib'' 2 = 1
        fib'' n = fib' (n-1) + fib' (n-2) + 1
