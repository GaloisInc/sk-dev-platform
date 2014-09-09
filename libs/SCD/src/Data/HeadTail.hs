{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
    FlexibleInstances #-}
-- | The HeadTail class used for parsing with either strings or bytestrings.
module Data.HeadTail(HeadTail(..)) where

import qualified Data.ByteString.Lazy.Char8 as BS

class HeadTail l a | l -> a where
  headTail :: l -> Maybe (a,l)

instance HeadTail [a] a where
  headTail [] = Nothing
  headTail (x:xs) = Just (x,xs)

instance HeadTail BS.ByteString Char where
  headTail s | BS.null s    = Nothing
             | otherwise = Just (BS.head s, BS.tail s)
