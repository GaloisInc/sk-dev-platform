{-# OPTIONS_GHC -Wall #-}
module SCD.Lobster.Symbolic.Tree
    ( Tree ( .. )         -- Rose tree
    , findNode            -- Find node storing a value in a tree, Breadth First Search
    , findP               -- Find children with property, including current node
    , findChildren        -- Find children with property, not including current node
    , findChild           -- Choose one of the descendents with the property
    , updateChild         -- Update first child with specified property
    , updateForest        -- Update all the trees in a forest
    , updateFirstInForest -- Update first child to match condition
    , replace             -- Replace a tree in a forest
    , replaceVal          -- Replace the value of a tree in a forest
    ) where

-- import Data.List  ( find )
import Data.Maybe ( catMaybes )
import Data.List  ( concatMap )

data Tree a = Node { val    :: a
                   , forest :: Forest a
                   } deriving ( Eq, Ord, Show )

type Forest a = [ Tree a ]

isNode :: ( Eq a ) => a -> Tree a -> Bool
isNode a t = val t == a

findNode :: ( Eq a ) => a -> Tree a -> Maybe ( Tree a )
findNode a n = if isNode a n
               then Just n
               else let rs = map ( findNode a ) ( forest n )
                        rs' = catMaybes rs
                    in if length rs' == 0
                       then Nothing
                       else Just ( head rs' )

findP :: ( a -> Bool ) -> Tree a -> [ Tree a ]
findP p t = if p ( val t )
            then t : concatMap ( findP p ) ( forest t )
            else concatMap ( findP p ) ( forest t )

findChildren :: ( a -> Bool ) -> Tree a -> [ Tree a ]
findChildren p t = concatMap ( findP p ) ( forest t )

findChild :: ( a -> Bool ) -> Tree a -> Maybe ( Tree a )
findChild p t = let kids = findChildren p t
                in if null kids
                   then Nothing
                   else Just ( head kids )

updateRoot :: ( a -> a ) -> Tree a -> Tree a
updateRoot f t = t { val = f ( val t ) }

updateForest :: ( a -> a ) -> Forest a -> Forest a
updateForest f ts = map ( updateRoot f ) ts

updateChild :: ( a -> Bool ) -> ( a -> a ) -> Tree a -> Tree a
updateChild p f t =
  if p ( val t )
  then updateRoot f t
  else t { forest = updateFirstInForest p f ( forest t ) }

-- This should work for the use cases, eventually need to make it recursive
updateFirstInForest :: ( a -> Bool ) -> ( a -> a ) -> [ Tree a ] -> [ Tree a ]
updateFirstInForest _p _f [ ] = [ ]
updateFirstInForest p f ( t : ts )=
  if p ( val t ) then updateRoot f t : ts else t : updateFirstInForest p f ts

-- Replace a tree in a forest
replace :: ( Tree a -> Tree a -> Bool ) -> Tree a -> Forest a -> Forest a
replace _p _new [ ] = [ ]
replace p new ( t : ts ) =
  if p new t
  then new : ts
  else t : ( replace p new ts )

-- Replace the value at a tree in a forest
replaceVal :: ( a -> a -> Bool ) -> a -> Forest a -> Forest a
replaceVal _p _newVal [ ] = [ ]
replaceVal p newVal ( t : ts ) =
  if p newVal ( val t )
  then ( t { val = newVal } ) : ts
  else t : ( replaceVal p newVal ts )
