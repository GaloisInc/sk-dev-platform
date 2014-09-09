{-# LANGUAGE RankNTypes, TypeOperators #-}

-- | A class that uses the Generics language extension for generating
-- random first-order data.  To use it for a datatype @T@, simply do @instance Generate T@.

module Test.QuickCheck.Generate(
   Generate(..)
 , generateArbitrary
 ) where

#ifndef __HADDOCK__
import GHC.Base(Unit(..), (:*:)(..), (:+:)(..))
#endif
import System.Random(StdGen, randomR, split, random, next)
import Test.QuickCheck.Gen ( sized, Gen(..) )
import Data.Word(Word8, Word16)

-- | Generate random data of given size.  Has generic default methods.
class Generate a where
  maxLn :: Int -> a -> Int
#ifndef __HADDOCK__
  maxLn {| Unit |}    _ _ = 1
  maxLn {| a :*: b |} 1 _ = 1
  maxLn {| a :*: b |} m _ = 1+u+v
                             where u = maxLn (m-1) (undefined::a)
                                   v = maxLn (max 1 (m-1-u)) (undefined::b)
  maxLn {| a :+: b |} 1 _ = 1
  maxLn {| a :+: b |} m _ = 1+max u v
                             where u = maxLn (m-1) (undefined :: a)
                                   v = maxLn (m-1) (undefined :: b)

#endif
  minLn :: Int -> a -> Int
#ifndef __HADDOCK__
  minLn {| Unit |}    _ _ = 1
  minLn {| a :*: b |} 1 _ = 1
  minLn {| a :*: b |} m _ = 1+u+v
                             where u = minLn (m-1) (undefined::a)
                                   v = minLn (max 1 (m-1-u)) (undefined::b)
  minLn {| a :+: b |} 1 _ = 1
  minLn {| a :+: b |} m _ = 1+min u v
                             where u = minLn (m-1) (undefined :: a)
                                   v = minLn (m-1) (undefined :: b)

#endif
  generate :: Int -> StdGen -> a
#ifndef __HADDOCK__
  generate {| Unit |}     _ _ = Unit
  generate {| a :*: b |} m r = generate ml rl :*: generate mr rr
                        where l1 = max mina (min maxa (m - maxb - 2))
                              l2 = min maxa (m - minb - 2)
                              (ml,r2) = randomR (l1,l2) r
                              (rl,rr) = split r2
                              mr = max minb (min maxb (m - ml - 1))
                              maxa = c (maxL (undefined::a))
                              mina = minL (undefined::a)
                              maxb = c (maxL (undefined::b))
                              minb = minL (undefined::b)

  generate {| a :+: b |} m r | (m > maxa || m > maxb) && maxa > maxb  = left
                       | (m > maxa || m > maxb) && maxa < maxb  = right
                       | (m < mina || m < minb) && mina < minb  = left
                       | (m < mina || m < minb) && mina > minb  = right
                       | l                                      = left
                       | otherwise                              = right
                        where left  = Inl (generate (m-1) rl)
                              right = Inr (generate (m-1) rr)
                              (l,r2) = random r
                              (rl,rr) = split r2
                              maxa = c (maxL (undefined::a)+1)
                              mina = minL (undefined::a)+1
                              maxb = c (maxL (undefined::b)+1)
                              minb = minL (undefined::b)+1
#endif

instance Generate ()
instance Generate Bool
instance Generate a => Generate (Maybe a)
instance Generate a => Generate [a]
instance (Generate a, Generate b) => Generate (a,b)
instance (Generate a, Generate b) => Generate (Either a b)
instance Generate Word16 where
  generate _ r = fromIntegral (fst (next r))
  minLn _ _ = 2
  maxLn _ _ = 2

instance Generate Word8 where
  generate _ r = fromIntegral (fst (next r))
  minLn _ _ = 2
  maxLn _ _ = 2

rand :: Gen StdGen
rand = MkGen (\r n -> r)

generateArbitrary :: Generate a => Gen a
generateArbitrary =
  do r <- rand
     s <- sized return
     return (generate s r)

-- Local

cutoff :: Int
cutoff = 20

maxL :: Generate a => a -> Int
maxL = maxLn cutoff

c :: Int -> Int
c x = if x >= cutoff then maxBound else x

minL :: Generate a => a -> Int
minL = minLn cutoff

class G a where
  gen :: [[a]]
#ifndef __HADDOCK__
  gen {| Unit |}    = [[Unit]]
  gen {| a :+: b |} = zipL (\a b -> map Inl a ++ map Inr b) gen gen
  gen {| a :*: b |} = []:diagonal (\a b -> [(x :*: y) | x <- a, y <- b]) gen gen
#endif

diagonal :: (a -> b -> [c]) -> [a] -> [b] -> [[c]]
diagonal f (a:as) bs = zipL (++) (map (f a) bs) ([]:diagonal f as bs)
diagonal _ []     _ = []

zipL :: ([a] -> [b] -> c) -> [[a]] -> [[b]] -> [c]
zipL f [] [] = []
zipL f (x:xs) (y:ys) = f x y : zipL f xs ys
zipL f l []          = map (flip f []) l
zipL f [] l          = map (f []) l

instance G Bool
instance G a => G [a]
instance G a => G (Maybe a)
instance (G a, G b) => G (a,b)
instance (G a, G b) => G (Either a b)
instance G Integer where
  gen = take 40 ([0]:[[x,-x] | x <- fibs])

fibs = 1:1:f fibs where f ~(a:b:l) = a+b : f (b:l)
