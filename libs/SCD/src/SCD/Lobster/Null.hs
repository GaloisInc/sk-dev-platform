module Null
    ( Null (..)   -- Class with null value
    , EqNull (..) -- Class with null value and test for null value
    ) where

import Data.Word ( Word32, Word64 )

----------------------------------------------------------------------
-- Class having a reasonable null value
----------------------------------------------------------------------

class Null n where
    mkNull :: n     -- Produce a null value

-- Types with equality and null values
class (Eq n, Null n) => EqNull n where
    isNull :: n -> Bool
    isNull = \n -> n == mkNull

instance Null Int where
    mkNull = 0

instance EqNull Int

instance Null Integer where
    mkNull = 0

instance EqNull Integer

instance Null [a] where
    mkNull = []

instance (Eq a) => EqNull [a]

instance Null (Maybe n) where
    mkNull = Nothing

instance (Eq n) => EqNull (Maybe n)

instance Null () where
    mkNull = ()

instance EqNull ()

-- When the codomain of a mapping has a null value, then we can
-- construct a null value for the mapping by associating the null
-- value of the codomain with each domain value.
instance (Null b) => Null (a -> b) where
    mkNull = const mkNull

-- No instance of equality for functions a -> b, so no instance
-- of EqNull for functions a -> b

instance Null Word64 where
    mkNull = 0

instance Null Word32 where
    mkNull = 0
