{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  Lobster.Gen.Perm
Description :  Working with SELinux reference policy permissions
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

Associated with a target resource in an SELinux access vector is
a pair -- the class of object/resource and the permission granted
(or denied.) This module provides some functions for working with
such permission values, as represented by @SELinux.Syntax@.

-}
module SCD.Lobster.Gen.Perm 
       ( PermName
       , PermissionMap
       
       , emptyPermissionMap   -- :: PermissionMap
       , buildPermissions     -- :: M4.Policy -> PermissionMap
       , expandPermission     -- :: PermissionMap -> PermName -> [PermName]
       
       ) where

import qualified SCD.SELinux.Syntax as SE
import qualified SCD.M4.Syntax as M4

import Data.Foldable ( toList )
import qualified Data.Map as M

type PermName      = String
type PermissionMap = M.Map PermName [PermName]

emptyPermissionMap :: PermissionMap
emptyPermissionMap = M.empty

buildPermissions :: M4.Policy -> PermissionMap
buildPermissions p = 
 foldr addPPerm
       (foldr addCPerm emptyPermissionMap (M4.commonPerms p))
       (M4.classPermissionDefs p)
 where
  addPPerm (M4.ClassPermissionDef cid nls _) m = 
    M.insert (SE.idString $ SE.toId cid)
             (map (SE.idString . SE.toId) (toList nls)) m

  addCPerm (SE.CommonPerm cid nls) m = 
    M.insert (SE.idString $ SE.toId cid)
             (map (SE.idString . SE.toId) (toList nls)) m
  
expandPermission :: PermissionMap -> PermName -> [PermName]
expandPermission pMap nm = 
   case snd $ go ([nm],[]) nm of
     [] -> [nm]
     xs -> xs
  where
   go (seen,acc) p = 
    case M.lookup p pMap of
      Nothing 
        | not (p `elem` seen) -> (p:seen, p:acc)
	| otherwise     -> (seen, acc)
      Just ps ->
       case filter (not.(`elem` acc)) ps of
         []  -> (seen,acc)
	 ps1 -> foldl go (p:seen,acc) ps1
      
