{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : $Header$
Description : Shrimp error types and pretty-printing
Copyright   : (c) Galois, Inc.

Shrimp error types and pretty-printing
-}

module SCD.M4.Errors(Error(..), nubError, occurrences, flatten, distribute,
  errorsByType) where

import SCD.M4.Syntax(IfdefId, M4Id, LayerModule, ModuleId)

import SCD.SELinux.Syntax(Identifier, ClassId, CommonId, PermissionId,
  SignedId, CondExpr, pos)

import SCD.M4.PShow(PShow, pShow, pShowLayerModule, showPos)

import SCD.M4.Kind(Kind, ParameterMap, ParameterInfo, ParameterKind,
  ppParameters)

import Text.PrettyPrint.HughesPJ(text, (<+>), (<>), colon, ($+$),
  quotes)

import Text.PrettyPrint.Pp(Pp, pp, pnest, above)

import Data.Set(Set)
import Data.Map(assocs, empty, insertWith, Map)
import Data.Foldable(toList)
import Data.List           (intersperse, nub)
import Data.List.GroupSort (groupSort)

import Data.Generics(Data, Typeable, toConstr, constrIndex, ConIndex)

import Prelude hiding(FilePath)
import qualified Prelude

data Occ a = Occ ConIndex a

instance Eq (Occ a) where
  a == b = compare a b == EQ

instance Ord (Occ a) where
  Occ a _ `compare` Occ b _ = a `compare` b

occurrences :: Data a => [a] -> [(Integer,a)]
occurrences as = 
  [(i,a) | (Occ _ a, i) <- assocs (foldr occ empty [Occ (constrIndex (toConstr a)) a | a <- as])] 
  where occ :: (Eq a, Ord a) => a -> Map a Integer -> Map a Integer
        occ a = insertWith (+) a 1

data Error =
    DuplicateAccessVectorDefinition ClassId ClassId
  | DuplicateClassPermission PermissionId ClassId
  | DuplicateCommonDef CommonId CommonId
  | DuplicateCommonPermission PermissionId CommonId
  | DuplicateDefinitions [M4Id]
  | DuplicateIdentifier Identifier Identifier
  | DuplicateMacroDef Identifier Identifier
  | DuplicateOccurrences M4Id Identifier
  | DuplicateSymbolDeclaration Identifier Kind Identifier
  | ErrorsIn Error [Error]
  | FragmentKindError [SignedId Identifier] (Set ParameterKind)
  | IllegalFragment String Identifier
  | IllegalMacroUse Identifier Identifier
  | IllegalParameterUse ParameterMap
  | IllegalSymbolDeclarations (Set Identifier)
  | IllegalSymbolReference Identifier [Identifier]
  | InconsistentMacroDefinitions [Identifier]
  | InconsistentSymbolDefinitions [(Identifier, Kind)]
  | InDefinition M4Id
  | InImplementation LayerModule
  | MissingAccessVectorDefinition ClassId
  | MissingModuleConfig ModuleId
  | ModuleIdMismatch ModuleId Prelude.FilePath
  | MutuallyRecursive [[M4Id]]
  | RefPolicyWarnCall Identifier [String]
  | KindMismatch Kind (Set Kind) Identifier
  | UndefinedCall M4Id
  | UndefinedCommonId CommonId
  | UndefinedIdentifier Identifier
  | UndefinedIds [(Identifier, Kind)]
  | UndocumentedParameters M4Id [ParameterInfo]
  | UnknownIfdefId IfdefId
  | UnknownModuleConfigNames (Set ModuleId)
  | UnusedArguments M4Id [ParameterInfo]
  | WhenTunableTrue CondExpr
  | WhenTunableFalse CondExpr
  | WrongNumberOfArguments M4Id [ParameterInfo] Identifier
  deriving (Eq, Ord, Show, Typeable, Data)

flatten :: [Error] -> [Error]
flatten = flip flat [] where
  flat (ErrorsIn _ es:l) r = flat es (flat l r)
  flat (e:l)             r = e:flat l r
  flat []                r = r

nubError :: [Error] -> [Error]
nubError es = nub $ map nubErrorsIn es

nubErrorsIn :: Error -> Error
nubErrorsIn (ErrorsIn x es) = ErrorsIn x $ nubError es
nubErrorsIn x               = x

distribute :: [Error] -> [Error]
distribute = concatMap dist where
  dist :: Error -> [Error]
  dist (ErrorsIn e es) = map (ErrorsIn e . (:[])) (distribute es)
  dist e               = [e]

unite :: [Error] -> [Error]
unite (ErrorsIn e es:ErrorsIn e' es':es'') | e == e'  = unite (ErrorsIn e (es++es'):es'')
unite (ErrorsIn e es:es')                             = ErrorsIn e (unite es):unite es'
unite (e:es)                                          = e:unite es
unite []                                              = []

deepIndex :: Error -> ConIndex
deepIndex (ErrorsIn _ [e]) = deepIndex e
deepIndex (ErrorsIn _ _)   = error "deepIndex"
deepIndex e = constrIndex (toConstr e)

errorsByType :: [Error] -> [[Error]]
errorsByType = map unite . map snd . groupSort deepIndex . distribute

instance Pp Error where
  pp (DuplicateAccessVectorDefinition c oc) = text "Duplicate access-vector definition:" <+> pShow c
                                                   $+$ pnest (text "defined at" <+> pShow oc)
  pp (DuplicateClassPermission p _c)        = text "Duplicate permission:" <+> pShow p
  pp (DuplicateCommonDef c oc)              = text "Duplicate common definition:" <+> pShow c
                                                   $+$ pnest (text "defined at" <+> pShow oc)
  pp (DuplicateCommonPermission p _c)       = text "Duplicate permission:" <+> pShow p
  pp (DuplicateDefinitions mds)             = text "Duplicate definitions:" <+> pShow mds
  pp (DuplicateIdentifier i oi)             = text "Duplicate identifier:" <+> pShow i
                                                    $+$ pnest (text "defined at" <+> pShow oi)
  pp (DuplicateMacroDef i oi)               = text "Duplicate definition of macro:" <+> pShow i
                                                    $+$ pnest (text "defined at" <+> pShow oi)
  pp (DuplicateOccurrences _i p)            = text "Duplicate occurrences of identifier: " <+> pShow p
  pp (DuplicateSymbolDeclaration i k oi)    = text "Duplicate symbol declaration:" <+> pShow (i,k)
                                                    $+$ pnest (text "defined at" <+> pShow oi)
  pp (ErrorsIn e es)                        = pp e <> colon $+$ pnest (above (intersperse (text "") (map pp es)))
  pp (FragmentKindError a fk)               = text "Kind error: expected fragment parameter of kind" <+> pShow fk <> text "but saw complex parameter:" <+> pShow a
  pp (IllegalFragment s i)                  = text "Fragment" <+> quotes (text s) <+> text "is defined as a macro:" <+> pShow i
  pp (IllegalMacroUse i oi)                 = text "Illegal use of macro:" <+> pShow i $+$ pnest (text "defined at" <+> pShow oi)
  pp (IllegalParameterUse ps)               = text "Illegal use of parameters in implementation:" <+> text (show ps)
  pp (IllegalSymbolDeclarations is)         = text "Illegal symbol declarations in interface:" <+> pShow (toList is)
  pp (IllegalSymbolReference i is)          = text "Illegal symbol reference across modules:" <+> pShow i 
                                                   $+$ pnest (if null is then text "is undefined." else text "is defined at" <+> pShow is)
  pp (InconsistentMacroDefinitions ms)      = text "Inconsistent macro definitions (not defined in both branches of an ifdef):" <+> pShow ms
  pp (InconsistentSymbolDefinitions is)     = text "Inconsistent symbol declarations (not defined in both branches of an ifdef):" <+> pShow is
  pp (InDefinition i)                       = text "In definition of" <+> pShow i
  pp (InImplementation lm)                  = text "In implementation of" <+> pShowLayerModule lm
  pp (MissingAccessVectorDefinition c)      = text "Missing access-vector definition for" <+> pShow c
  pp (MissingModuleConfig mi)               = text "Missing module configuration for" <+> pShow mi
  pp (ModuleIdMismatch mi m)                = text "File base name" <+> text m <+> text "doesn't match module name:" <+> pShow mi
  pp (MutuallyRecursive mcs)                = text "Mutually recursive definitions:" <+> pShow mcs
  pp (RefPolicyWarnCall i ws)               = text "Call to macro with refpolicywarnings:" <+> pShow i $+$ pnest (above (map text ws))
  pp (KindMismatch k ks i)                  = text "Kind mismatch: expected" <+> pShow k <> text ", got" <+> pShow ks <> colon <+> pShow i
  pp (UndefinedCall i)                      = text "Call to undefined macro:" <+> pShow i
  pp (UndefinedCommonId c)                  = text "Undefined commonId:" <+> pShow c
  pp (UndefinedIdentifier p)                = text "Undefined identifier:" <+> pShow p
  pp (UndefinedIds is)                      = text "Undefined identifiers (need to be declared or put in require block):" <+> pShow is
  pp (UndocumentedParameters _i p)          = text "Undocumented parameters (missing parameter names) in" <+> ppParameters p
  pp (UnknownIfdefId i)                     = text "Unknown identifier in ifdef:" <+> pShow i
  pp (UnknownModuleConfigNames is)          = text "Unknown module identifiers:" <+> pShow (toList is)
  pp (UnusedArguments _i u)                 = text "Unused parameters (with kind {any}) in" <+> ppParameters u
  pp (WhenTunableTrue c)                    = text "When" <+> pp c
  pp (WhenTunableFalse c)                   = text "When not(" <> pp c <> text ")"
  pp (WrongNumberOfArguments i pks oi)      = text "Wrong number of arguments:" <+> pp i <> ppParameters pks <+> showPos (pos i) 
                                                   $+$ pnest (text "defined at" <+> pShow oi)

