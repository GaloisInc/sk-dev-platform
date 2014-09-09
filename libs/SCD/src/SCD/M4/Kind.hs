{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

{- |
Module      : $Header$
Description : Kind data constructors and pretty-printing
Copyright   : (c) Galois, Inc.

Kind data constructors and pretty-printing
-}

module SCD.M4.Kind
       ( Kind(..)

       , ParameterMap
       , ParameterKind(..)
       , ParameterInfo(..)
       , ParameterIndex(..)
       , ppParameters
       
       , Fragment(..)

       , HasKind(..)
       , PosKind(..)
       , posKind
       , posKind'
       , getKind
       
       , KindMap
       , KindMaps(..)
       , emptyKindMaps
       
       , AllowMap
       , AllowInfo(..)
       , allowInfo

       , M4Info(..)

       , InterfaceEnv(..)
       , emptyInterfaceEnv
       ) where

import SCD.M4.Syntax(IfdefId, LevelId, MlsRange(..), Parameter,
  RefPolicyWarnLine(..))

import SCD.SELinux.Syntax(Identifier, ClassId,
  RoleId, PermissionId, TypeId, AttributeId,
  TypeOrAttributeId, UserId, BoolId, Sid, NetInterfaceId,
  FileSystemId, pos)

import Text.Happy.ParserMonad(Pos, noPos)


import SCD.M4.PShow(PShow, pShow)

import SCD.M4.PrettyPrint()

import qualified Data.Map as Map
import Data.Set(Set)
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Generics(Data, Typeable)
import Data.Foldable(toList)

import Text.PrettyPrint.HughesPJ(text, (<>), empty, punctuate,
  comma, parens, hsep, Doc, (<+>), colon, braces)

import Text.PrettyPrint.Pp(Pp, pp, underline, questionmark, dollar)

---

data Kind =
   ClassKind
 | PermissionKind
 | TypeKind
 | AttributeKind
 | TypeOrAttributeKind
 | SidKind
 | BoolKind
 | UserKind
 | RoleKind
 | NetInterfaceKind
 | FileSystemKind
 | IfdefKind
 | LevelKind
 | MlsRangeKind
 | DomainKind
 | AnyKind
  deriving (Typeable, Data, Eq, Ord, Read, Show)

instance PShow Kind
  where pShow = pp

instance Pp Kind where
 pp ClassKind           = text "class"
 pp PermissionKind      = text "permission"
 pp TypeKind            = text "type"
 pp AttributeKind       = text "attribute"
 pp TypeOrAttributeKind = text "type/attribute"
 pp SidKind             = text "sid"
 pp BoolKind            = text "bool"
 pp UserKind            = text "user"
 pp RoleKind            = text "role"
 pp NetInterfaceKind    = text "netinterface"
 pp FileSystemKind      = text "filesystem"
 pp IfdefKind           = text "ifdef"
 pp LevelKind           = text "level"
 pp MlsRangeKind        = text "mlsrange"
 pp DomainKind          = text "domain"
 pp AnyKind             = text "any"

class HasKind i where 
  iKind :: i -> Kind

instance HasKind ClassId           where iKind _ = ClassKind
instance HasKind PermissionId      where iKind _ = PermissionKind
instance HasKind TypeId            where iKind _ = TypeKind
instance HasKind AttributeId       where iKind _ = AttributeKind
instance HasKind TypeOrAttributeId where iKind _ = TypeOrAttributeKind
instance HasKind Sid               where iKind _ = SidKind
instance HasKind BoolId            where iKind _ = BoolKind
instance HasKind UserId            where iKind _ = UserKind
instance HasKind RoleId            where iKind _ = RoleKind
instance HasKind NetInterfaceId    where iKind _ = NetInterfaceKind
instance HasKind FileSystemId      where iKind _ = FileSystemKind
instance HasKind IfdefId           where iKind _ = IfdefKind
instance HasKind LevelId           where iKind _ = LevelKind
instance HasKind MlsRange          where iKind _ = MlsRangeKind

newtype ParameterIndex = ParameterIndex Int
  deriving (Typeable, Data, Eq, Ord, Num, Enum, Read, Show)

instance PShow ParameterIndex where
  pShow (ParameterIndex i) = dollar <> text (show i)

data ParameterKind = ParameterKind{ fragment :: Fragment
                                  , kind     :: Kind
                                  } 
  deriving (Typeable, Data, Eq, Ord, Read, Show)

instance Pp ParameterKind where
  pp (ParameterKind f k) = 
       pp k <> (if f == Fragment then underline else empty)

data ParameterInfo = ParameterInfo{ name           :: Maybe String
                                  , optional       :: Bool
                                  , parameterKinds :: Set ParameterKind
                                  }
  deriving (Typeable, Data, Eq, Ord, Read, Show)

instance Pp ParameterInfo where
  pp (ParameterInfo n o p) = maybe empty text n <> 
                             (if o then questionmark else empty) <+> 
                             (if isJust n then colon else empty) <+>
                             braces (hsep (punctuate comma (map pp (toList p))))

ppParameters :: [ParameterInfo] -> Doc
ppParameters s = parens (hsep (punctuate comma (map pp s)))

instance PShow Parameter
  where pShow p = text "\n" <> pp p

data Fragment =
   Fragment 
 | Whole
  deriving (Typeable, Data, Eq, Ord, Read, Show)

data PosKind 
 = PosKind { pkPos  :: Pos
           , pkKind :: Kind
	   }
  deriving Show

instance Eq PosKind where
  p1 == p2 = pkKind p1 == pkKind p2

instance Ord PosKind where
  compare p1 p2 = compare (pkKind p1) (pkKind p2)

posKind :: Identifier -> Kind -> PosKind
posKind i k = PosKind{pkPos=pos i, pkKind=k}

posKind' :: Kind -> PosKind
posKind' k = PosKind{pkPos=noPos,pkKind=k}

getKind :: PosKind -> Kind
getKind pk = pkKind pk

type KindMap = Map Identifier (Set PosKind)

type ParameterMap = Map ParameterIndex (Set ParameterKind)

data AllowInfo
 = AllowInfo
    { avTarget  :: Identifier
    , avClasses :: [Identifier]
    , avPerms   :: [Identifier]
    } deriving ( Eq, Show )
    
instance Ord AllowInfo where
  compare a1 a2 = compare (avTarget a1) (avTarget a2)

allowInfo :: Identifier -> [Identifier] -> [Identifier] -> AllowInfo
allowInfo t cs ps = AllowInfo
    { avTarget  = t
    , avClasses = cs
    , avPerms   = ps
    }

type AllowMap = Map Identifier (Set AllowInfo)

data M4Info = 
   M4Macro InterfaceEnv
 | M4IdSet (Set Kind) (Set Identifier) (Maybe RefPolicyWarnLine)
 | M4Ifdef (Maybe Bool)
  deriving Show

data KindMaps = KindMaps{ inputMap    :: KindMap
                        , iInputMap   :: KindMap
                        , outputMap   :: KindMap
                        , iOutputMap  :: KindMap
                        , localMap    :: KindMap
			, allowMap    :: AllowMap
                        }
  deriving Show

emptyKindMaps :: KindMaps
emptyKindMaps = KindMaps{ inputMap    = Map.empty
                        , iInputMap   = Map.empty
                        , iOutputMap  = Map.empty
                        , outputMap   = Map.empty
                        , localMap    = Map.empty
			, allowMap    = Map.empty
                        }

data InterfaceEnv = InterfaceEnv{ kindMaps :: KindMaps
                                , refpolicywarn :: [String]
                                , parameterInfo :: [ParameterInfo]
                                }
  deriving Show


emptyInterfaceEnv :: InterfaceEnv
emptyInterfaceEnv = InterfaceEnv{ kindMaps = emptyKindMaps
                                , refpolicywarn = []
                                , parameterInfo = []
                                }
