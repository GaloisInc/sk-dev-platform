{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -XCPP #-}
{- |
Module      : $Header$
Description : Representing kind information for SELinux policies.
Copyright   : (c) Galois, Inc.

Representing kind information for SELinux policies + utility
functions and combinators for working with that info.
-}
module SCD.M4.KindInfo
       ( KindInfo(..)
       , emptyKindInfo

       , xrefOutputMaps
       , xrefPolicyModule
       , xrefInterface

       , kindMapElems

       , IdXrefMap
       , xrefKindMap
       , simpleKindMaps
       , simpleKindSet

       , simpleParameterInfo
       , simpleParameterKind
       , sameExceptKind
       , findSuperKinds
       , superKinds

       , portRefs

       , normalizeId
       , Fragments
       , fragments
       , wholeFragments
       , staticFragments
       ) where

import SCD.M4.Kind
import SCD.M4.Util

import SCD.M4.Syntax(M4Id, ModuleId,
  LayerModule, PolicyModule(..), Interface(..),
  InterfaceElement(..))

import qualified Lobster.Abs as L
import SCD.Lobster.Util()

import SCD.SELinux.Syntax(Identifier, IsIdentifier(..),
  withIdString)

import Data.Map(Map, lookup, assocs, fromList, keysSet)
import Prelude hiding (FilePath, lookup, foldr)
import qualified Data.MapSet as MapSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Foldable(Foldable, toList, foldr)

-----------


type IdXrefMap = Map Identifier
                    (Set ( Identifier  --  same as key (redundant)
                         , LayerModule -- defining module
                         , Bool        -- True: directly defined in that module
                         , Kind
                         , Maybe M4Id  -- defining template (if any)
                         ))

data KindInfo
  = KindInfo{ interfaceEnv :: InterfaceEnv
            , m4Env        :: Map Identifier M4Info
            , implEnv      :: Map ModuleId KindMaps
            , xrefs        :: IdXrefMap
            , parameterMap :: ParameterMap
            }
  deriving Show

emptyKindInfo :: KindInfo
emptyKindInfo = KindInfo
     { interfaceEnv = emptyInterfaceEnv
     , m4Env        = Map.empty
     , implEnv      = Map.empty
     , xrefs        = Map.empty
     , parameterMap = Map.empty
     }

kindMapElems :: KindMap -> [(Identifier, Kind)]
kindMapElems km = concat [ [ (mkId' p (idString i),k)
                           | PosKind p k <- toList ks ]
                         | (i,ks) <- assocs km ]


-- | Join all iOutputMaps/outputMaps to get a map for cross references
xrefOutputMaps :: KindInfo -> [PolicyModule] -> IdXrefMap
xrefOutputMaps ki = foldr MapSet.union Map.empty . map (xrefPolicyModule ki)

xrefPolicyModule :: KindInfo -> PolicyModule -> IdXrefMap
xrefPolicyModule ki pm =
  foldr MapSet.union Map.empty
     (map (xrefInterface ki lm) ifs)
  `MapSet.union`
  maybe Map.empty
        (xrefKindMap Nothing lm)
        (lookup (implementationId (implementation pm)) (implEnv ki))

  where InterfaceModule _ ifs = interface pm
        lm = layerModule pm

xrefInterface :: KindInfo -> LayerModule -> InterfaceElement -> IdXrefMap
xrefInterface ki lm (InterfaceElement _ _ i _) =
  case lookup (toId i) (m4Env ki) of
    Just (M4Macro ie) -> xrefKindMap (Just i) lm (kindMaps ie)
    _ -> Map.empty

xrefKindMap :: (Maybe M4Id) -> LayerModule -> KindMaps -> IdXrefMap
xrefKindMap mi lm km = xrefKindMapO False mi lm km `MapSet.union`
                       xrefKindMapO True  mi lm km

xrefKindMapO :: Bool -> (Maybe M4Id) -> LayerModule -> KindMaps -> IdXrefMap
xrefKindMapO orm mi lm km = fromList
  [ (nti, Set.singleton (nti, lm, orm, k, mi))
  | (ti, k) <- kindMapElems kMap,
    nti     <- [normalizeId (toId ti)]
  ]
 where
  kMap
   | orm       = outputMap km
   | otherwise = iOutputMap km

-- | If an identifier in (i)InputMap is a domain and the identifier is
-- also in (i)OutputMap or localMap, then move the domain info from
-- (i)InputMap to (i)OutputMap or localMap, respectively.
simpleKindMaps :: KindMaps -> KindMaps
simpleKindMaps km = KindMaps{ inputMap    = imap
                            , iInputMap   = idmap
                            , outputMap   = ormap
                            , iOutputMap  = omap
                            , localMap    = lmap
			    , allowMap    = allowMap km
                            }
  where
  simple :: (KindMaps -> KindMap) -> (Set Identifier, KindMap)
  simple s = (ds,foldr (\i m' -> MapSet.insert i (posKind i DomainKind) m') m ds)
    where ds = Set.fromList [ i
                            | (i,k) <- kindMapElems m,
                              Set.member i dis,
                              k `elem` [TypeKind, AttributeKind]]
          m = s km
  (diso, omap)    = simple iOutputMap
  (disorm, ormap) = simple outputMap
  (disl, lmap)    = simple localMap
  disu = diso `Set.union` disorm `Set.union` disl
  imapf s = foldr (flip MapSet.delete (posKind' DomainKind)) (s km) disu
  imap = imapf inputMap
  idmap = imapf iInputMap
  disf s = keysSet (Map.filter (Set.member (posKind' DomainKind)) (s km))
  dis = disf inputMap `Set.union` disf iInputMap


simpleKindSet :: Set Kind -> Set Kind
simpleKindSet s = Set.filter (Set.null . Set.intersection s . findSuperKinds) s

simpleParameterInfo :: ParameterInfo -> ParameterInfo
simpleParameterInfo p = p{ parameterKinds = simpleParameterKind (parameterKinds p) }

simpleParameterKind :: Set ParameterKind -> Set ParameterKind
simpleParameterKind s =
  Set.filter (\p -> Set.null (Set.intersection (Set.map kind
                               (Set.filter (sameExceptKind p) s))
                               (findSuperKinds (kind p)))) s

sameExceptKind :: ParameterKind -> ParameterKind -> Bool
sameExceptKind (ParameterKind f _) (ParameterKind f' _ ) = f == f'

findSuperKinds :: Kind -> Set Kind
findSuperKinds = flip MapSet.lookup superKinds

superKinds :: Map Kind (Set Kind)
superKinds = MapSet.transitive $ fromList
            [ (TypeOrAttributeKind, Set.fromList [TypeKind, AttributeKind])
            , (TypeKind,            Set.fromList [DomainKind])
            , (AttributeKind,       Set.fromList [DomainKind])
            ]

---

-- | Generate a mapping from Lobster ports to implementation
--  identifiers, given a KindInfo of an analyzed domain (corresponding
--  to an implementation or template).  The output maps contain types
--  or attributes that are defined in the domain.  Since these types
--  may be referred to by other templates or interfaces, we provide
--  access to them by the generated ports.
--
-- Similarly, the local map and input maps contain types or attributes
-- that the domain refers to, but that are defined in other domains.
-- Again, the generated ports allows the unit to connect to those
-- other domains.
--
-- This is not complete, since we probably need to better distinguish
-- between ports that are used to access something defined inside a
-- domain from ports that are used to let the domain access something
-- in another domain.

portRefs :: KindInfo -> KindMaps -> Map L.PortId L.Identifier
portRefs ki kms =
   foldr (\ f acc -> pref (f kms) `Map.union` acc)
         Map.empty
	 [ outputMap
	 , iOutputMap
	 , inputMap
	 , iInputMap
	 , localMap
	 ]

  where pref km = fromList [(layerModule2Identifier lm,
                             layerModule2Identifier lm) | lm <- concat is]
          where is = [ findOrigin i
                     | (i,ks) <- assocs (Set.map getKind `fmap` km)
                     , not (Set.null (ks `Set.intersection` typekinds)) ]

        findOrigin i = take 1 [ lm | (_,lm, True, _,Nothing) <- toList xs ]
          where xs = MapSet.lookup (normalizeId i) (xrefs ki)

        typekinds = Set.fromList [TypeKind, AttributeKind,
                                  TypeOrAttributeKind, DomainKind]



---


-- NOTE: normalizeId doesn't account for nonlinear parameter use.
-- | Replace all parameters by $1, $2, $3, ...
normalizeId :: IsIdentifier i => i -> i
normalizeId = withIdString norm
  where norm s = f0 ++ concat ["$" ++ show (p :: Integer) ++ f |
                               (p,(_,f)) <- zip [1..] fs]
                 where (f0,fs) = fragments s


type Fragments = (String, [(ParameterIndex, String)])

fragments :: String -> Fragments
fragments = frag0 "" where
  frag0 a (i:r) | i /= '$' = frag0 (i:a) r
  frag0 a r = (reverse a, fragn r)

  fragn ('$':i:r) | i >= '1' && i <= '9' = frags "" (read [i]) r
  fragn _ = []

  frags a n (i:r) | i /= '$' = frags (i:a) n r
  frags a n r = (ParameterIndex n, reverse a):fragn r

wholeFragments :: Fragments -> Maybe ParameterIndex
wholeFragments ("",[(n,"")]) = Just n
wholeFragments _             = Nothing

staticFragments :: Fragments -> Maybe String
staticFragments (f,[]) = Just f
staticFragments _      = Nothing
