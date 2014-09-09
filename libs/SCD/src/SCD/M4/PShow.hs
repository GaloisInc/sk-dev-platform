{- |
Module      : $Header$
Description : Identifier pretty-printing
Copyright   : (c) Galois, Inc.

Identifier pretty-printing
-}

module SCD.M4.PShow(PShow(..), pShowIdentifier, showPos, pShowLayerModule) where

import SCD.M4.Syntax(IfdefId, LevelId, MlsRange, M4Id, LayerModule, ModuleId)

import SCD.SELinux.Syntax(Identifier, IsIdentifier(..), ClassId,
  CommonId, RoleId, PermissionId, TypeId, AttributeId,
  TypeOrAttributeId, UserId, BoolId, Sid, NetInterfaceId,
  FileSystemId, SignedId, signedId2Id)

import Text.Happy.ParserMonad(Pos(..))

import Prelude hiding (FilePath, mapM_, lookup)

import Data.NonEmptyList(NonEmptyList)

--import qualified Data.Set as Set
import Data.Set(Set)
import Data.Foldable(toList)

import Text.PrettyPrint.HughesPJ(Doc, text, (<>), (<+>), parens,
  colon, braces, sep, punctuate, comma)

import Text.PrettyPrint.Pp(Pp, pp, above, slash)

import SCD.M4.PrettyPrint()

class PShow a where
  pShow :: a -> Doc

pShowIdentifier :: (Pp i, IsIdentifier i) => i -> Doc
pShowIdentifier i = pp i <+> showPos (pos i)

showPos :: Pos -> Doc
showPos p = parens (text f <> colon <> text (show r)
                           <> slash <> text (show c))
  where Pos f _ r c = p


instance PShow Identifier        where pShow = pShowIdentifier
instance PShow ClassId           where pShow = pShowIdentifier
instance PShow PermissionId      where pShow = pShowIdentifier
instance PShow TypeId            where pShow = pShowIdentifier
instance PShow AttributeId       where pShow = pShowIdentifier
instance PShow TypeOrAttributeId where pShow = pShowIdentifier
instance PShow Sid               where pShow = pShowIdentifier
instance PShow BoolId            where pShow = pShowIdentifier
instance PShow UserId            where pShow = pShowIdentifier
instance PShow RoleId            where pShow = pShowIdentifier
instance PShow NetInterfaceId    where pShow = pShowIdentifier
instance PShow FileSystemId      where pShow = pShowIdentifier
instance PShow IfdefId           where pShow = pShowIdentifier
instance PShow LevelId           where pShow = pShowIdentifier
instance PShow CommonId          where pShow = pShowIdentifier
instance PShow M4Id              where pShow = pShowIdentifier
instance PShow ModuleId          where pShow = pShowIdentifier
instance PShow MlsRange where pShow = pp

instance PShow a => PShow [a] where
  pShow l = above (map pShow l)

instance PShow a => PShow (NonEmptyList a) where
  pShow l = pShow (toList l)

instance Pp a => PShow (Set a) where
  pShow s = braces (sep (punctuate comma (map pp (toList s))))

instance (Pp i, IsIdentifier i, PShow a) => PShow (i,a) where
  pShow (i, a) = pp i <> colon <+> pShow a <+> showPos (pos i)

instance PShow Doc where pShow = id

pShowLayerModule :: LayerModule -> Doc
pShowLayerModule lm = pp lm <+> showPos(pos (snd lm))

instance (Pp i, IsIdentifier i) => PShow (SignedId i) where
  pShow s = pp s <+> showPos (pos (signedId2Id s))
