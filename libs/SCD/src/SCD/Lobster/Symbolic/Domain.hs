{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.Domain
    ( Domain ( .. ) -- Information about a Lobster domain
    , Hierarchy     -- Hierarchy of domains leading to current point
    , empty         -- Empty hierarchy
    , ascend        -- Go up in the hierarchy
    , descend       -- Go down in the hierarchy
    , current       -- Current domain in the hierarchy
    , mkDomain      -- Make a domain structure
    , domainName    -- Get name of the domain
    ) where

data Domain = Domain String deriving ( Eq, Ord, Show )

mkDomain :: String -> Domain
mkDomain = Domain

domainName :: Domain -> String
domainName ( Domain n ) = n

--------------------------------------------------------------------------------
-- Model the hierarchy of domains, for the purpose of generating AST
--------------------------------------------------------------------------------

type Hierarchy = [ Domain ]

descend :: Domain -> Hierarchy -> Hierarchy
descend s h = s:h

ascend :: Hierarchy -> Hierarchy
ascend = tail

empty :: Hierarchy
empty = []

current :: Hierarchy -> Domain
current = head
