{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{- |
Module      : $Header$
Description : Abstract syntax for Shrimp M4 subset
Copyright   : (c) Galois, Inc.

Abstract syntax for Shrimp M4 subset
-}

module SCD.M4.Syntax(
   Policy(..)
 , PolicyModule(..)
 , Interface(..)
 , ModuleDoc(..)
 , InterfaceElement(..)
 , InterfaceType(..)
 , InterfaceDoc(..)
 , InfoFlow(..)
 , Flow(..)
 , Parameter(..)
 , Implementation(..)
 , Stmts
 , GenContext(..)
 , Require(..)
 , Stmt(..)
 , M4Var(..)
 , M4Id(..)
 , IfdefId(..)
 , LevelId(..)
 , LayerId(..)
 , ModuleId(..)
 , LayerModule
 , MlsRange(..)
 , Level
 , XMLDoc(..)
 , RefPolicyWarnLine(..)
 , Version(..)
 , FileContexts(..)
 , FileContext(..)
 , HomePath(..)
 , RegexpPath(..)
 , ClassPermissionDefs
 , ClassPermissionDef(..)
 , SupportDefs
 , SupportDef(..)
 , GlobalBooleans
 , GlobalBoolean(..)
 , BoolType(..)
 , ModuleConf(..)
 , ModuleConfSetting(..)
 , IfdefDecl(..)
   ) where

import SCD.SELinux.Syntax(Identifier, IsIdentifier(..), ClassId,
  RoleId, PermissionId, TypeId, AttributeId, TypeOrAttributeId,
  UserId, BoolId, Sid, CommonPerm, AvPerm, CondExpr(..), SignedId(..),
  NeverAllow(..), Self(..), Permissions(..), SourceTarget(..),
  Transition(..), AllowDeny(..), SidContext, FileSystemUse,
  GenFileSystemContext, PortContext, NetInterfaceContext, NodeContext,
  FileType, FilePath)

import Prelude hiding (FilePath)
import qualified Prelude

import Data.NonEmptyList(NonEmptyList)

import Text.XML.Light ( Content(..), Element(..), CData(..),
                      CDataKind(..) , QName(..)  , Attr(..), Line )

import Data.Generics(Typeable, Data)

#ifndef __HADDOCK__
deriving instance Read Content
deriving instance Read Attr
deriving instance Read Element
deriving instance Read CData
deriving instance Read CDataKind
deriving instance Read QName

deriving instance Eq Content
deriving instance Eq Element
deriving instance Eq CData

#if ! ( MIN_VERSION_xml(1,3,12) )
-- Up until xmnl-1.3.12, we needed these standalong instance dedclarations.
deriving instance Typeable Content
deriving instance Typeable Attr
deriving instance Typeable Element
deriving instance Typeable CData
deriving instance Typeable CDataKind
deriving instance Typeable QName

deriving instance Data Content
deriving instance Data Attr
deriving instance Data Element
deriving instance Data CData
deriving instance Data CDataKind
deriving instance Data QName
#endif
#endif

-- | M4 meta variables $0..$9, $*.
newtype M4Var = M4Var Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

-- | Macro identifiers defined in M4.
newtype M4Id = M4Id Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

-- TODO: perhaps collapse IfdefId with M4Id.
-- | M4-defined identifers used in M4 ifdef statements. 
newtype IfdefId = IfdefId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype LevelId = LevelId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype LayerId = LayerId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype ModuleId = ModuleId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

type LayerModule = (LayerId, ModuleId)

newtype XMLDoc = XMLDoc String
  deriving (Typeable, Data, Eq, Read, Show)

data Policy = Policy{ kernelClasses       :: [ClassId]
                    , userClasses         :: [ClassId]
                    , initialSids         :: NonEmptyList Sid
                    , commonPerms         :: [CommonPerm]
                    , avPerms             :: NonEmptyList AvPerm
                    , policyModules       :: [PolicyModule]
                    , classPermissionDefs :: ClassPermissionDefs
                    , supportDefs         :: SupportDefs
                    , globalBooleans      :: GlobalBooleans
                    , modulesConf         :: [ModuleConf]
                    , ifdefDecls          :: [IfdefDecl]
                    }
  deriving (Typeable, Data, Eq, Read, Show)

data PolicyModule = PolicyModule{ layerModule    :: LayerModule
                                , interface      :: Interface
                                , implementation :: Implementation
                                , fileContexts   :: FileContexts
                                , baseName       :: Prelude.FilePath
                                }
  deriving (Typeable, Data, Eq, Read, Show)

data Interface = InterfaceModule{ moduleDoc :: ModuleDoc
                                , interfaceElements :: [InterfaceElement]
                                }
  deriving (Typeable, Data, Eq, Read, Show)

data ModuleDoc = ModuleDoc
  { moduleSummary       :: String
  , moduleDescription   :: [Content]
  , required            :: Bool
  , requiredDescription :: String
  }
  deriving (Typeable, Data, Eq, Read, Show)

data InterfaceElement = InterfaceElement InterfaceType InterfaceDoc M4Id Stmts
  deriving (Typeable, Data, Eq, Read, Show)

data InterfaceType = 
   InterfaceType
 | TemplateType
  deriving (Typeable, Data, Eq, Ord, Read, Show)

data InterfaceDoc = InterfaceDoc
  { interfaceSummary     :: String
  , interfaceDescription :: [Content]
  , parameters           :: [Parameter]
  , infoFlow             :: Maybe InfoFlow
  }
  deriving (Typeable, Data, Eq, Read, Show)

data InfoFlow = InfoFlow{ flow :: Flow
                        , weight :: Int
                        } 
  deriving (Typeable, Data, Eq, Read, Show)

data Flow = Read | Write | Both | None
  deriving (Typeable, Data, Eq, Read, Show)


data Parameter = Parameter{ line :: Maybe Line
                          , name :: String
                          , parameterSummary :: String
                          , optional :: Bool
                          }
  deriving (Typeable, Data, Eq, Ord, Read, Show)



data Implementation = Implementation ModuleId Version Stmts
  deriving (Typeable, Data, Eq, Read, Show)

type Stmts = [Stmt]

data Require = 
   RequireClass ClassId (NonEmptyList PermissionId)
 | RequireRole (NonEmptyList RoleId)
 | RequireType (NonEmptyList TypeId)
 | RequireAttribute (NonEmptyList AttributeId)
 | RequireBool (NonEmptyList BoolId)
 | RequireIfdef IfdefId (NonEmptyList Require) [Require]
 | RequireIfndef IfdefId (NonEmptyList Require)
  deriving (Typeable, Data, Eq, Read, Show)

data Stmt =
   Tunable CondExpr Stmts Stmts
 | Optional Stmts Stmts
 | Ifdef IfdefId Stmts Stmts
 | Ifndef IfdefId Stmts
 | RefPolicyWarn RefPolicyWarnLine
 | Call M4Id [NonEmptyList (SignedId Identifier)]
 | Role RoleId [SignedId TypeOrAttributeId]
 | RoleTransition (NonEmptyList RoleId) (NonEmptyList (SignedId TypeOrAttributeId)) RoleId
 | RoleAllow (NonEmptyList RoleId) (NonEmptyList RoleId)
 | Attribute AttributeId
 | Type TypeId [TypeId] [AttributeId]
 | TypeAlias TypeId (NonEmptyList TypeId)
 | TypeAttribute TypeId (NonEmptyList AttributeId)
 | RangeTransition (NonEmptyList (SignedId TypeOrAttributeId)) 
                   (NonEmptyList (SignedId TypeOrAttributeId)) 
                   (NonEmptyList ClassId) MlsRange
 | TeNeverAllow (SourceTarget (NeverAllow TypeOrAttributeId) 
                              (NeverAllow Self))
                Permissions
 | Transition Transition (SourceTarget (NonEmptyList (SignedId TypeOrAttributeId))
                                       (NonEmptyList (SignedId TypeOrAttributeId))) 
                         TypeId
 | TeAvTab AllowDeny (SourceTarget (NonEmptyList (SignedId TypeOrAttributeId))
                                   (NonEmptyList (SignedId Self)))
                     Permissions
 | CondStmt CondExpr Stmts Stmts
 | XMLDocStmt XMLDoc
 | SidStmt (SidContext GenContext)
 | FileSystemUseStmt (FileSystemUse GenContext)
 | GenFileSystemStmt (GenFileSystemContext GenContext)
 | PortStmt (PortContext GenContext)
 | NetInterfaceStmt (NetInterfaceContext GenContext)
 | NodeStmt (NodeContext GenContext)
 | Define IfdefId
 | Require (NonEmptyList Require)
 | GenBoolean BoolType BoolId Bool
  deriving (Typeable, Data, Eq, Read, Show)

newtype RefPolicyWarnLine = RefPolicyWarnLine String
  deriving (Typeable, Data, Eq, Read, Show)

data MlsRange = MlsRange Level Level
  deriving (Typeable, Data, Eq, Read, Show)

data GenContext = GenContext UserId RoleId TypeId MlsRange
  deriving (Typeable, Data, Eq, Read, Show)

-- TODO: implement categories
type Level = LevelId

newtype Version = Version String
  deriving (Typeable, Data, Eq, Read, Show)

newtype FileContexts = FileContexts [FileContext] 
  deriving (Typeable, Data, Eq, Read, Show)

data FileContext = 
   FileContext HomePath (Maybe FileType) (Maybe GenContext)
 | FileContextIfdef IfdefId [FileContext] [FileContext]
 | FileContextIfndef IfdefId [FileContext]
  deriving (Typeable, Data, Eq, Read, Show)

data HomePath =
   HomeDir RegexpPath
 | HomeRoot RegexpPath
 | Path RegexpPath
  deriving (Typeable, Data, Eq, Read, Show)

data RegexpPath =
   PlainPath FilePath
 | RegexpPath String
  deriving (Typeable, Data, Eq, Read, Show)

type ClassPermissionDefs = [ClassPermissionDef]

data ClassPermissionDef = ClassPermissionDef M4Id (NonEmptyList Identifier) (Maybe RefPolicyWarnLine)
  deriving (Typeable, Data, Eq, Read, Show)

type SupportDefs = [SupportDef]

data SupportDef = SupportDef M4Id [Stmt]
  deriving (Typeable, Data, Eq, Read, Show)

type GlobalBooleans = [GlobalBoolean]

data BoolType = BoolType | TunableType
  deriving (Typeable, Data, Eq, Read, Show)

data GlobalBoolean = GlobalBoolean BoolType (Maybe XMLDoc) BoolId Bool
  deriving (Typeable, Data, Eq, Read, Show)

data ModuleConf = ModuleConf ModuleId ModuleConfSetting
  deriving (Typeable, Data, Eq, Read, Show)

data ModuleConfSetting =
   Base
 | Module
 | Off
  deriving (Typeable, Data, Eq, Read, Show)

data IfdefDecl = IfdefDecl IfdefId (Maybe Bool)
  deriving (Typeable, Data, Eq, Read, Show)
