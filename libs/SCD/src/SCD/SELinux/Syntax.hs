{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{- |
Module      :  $Header$
Description :  Abstract syntax for SELinux policies
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Abstract syntax for SELinux policies
-}
module SCD.SELinux.Syntax(
    Policy(..)
  , CommonPerm(..)
  , AvPerm(..)
  , TeRbac(..)
  , Stmt(..)
  , AvRuleBlock(..)
  , AvRule(..)
  , RequireStmt(..)
  , Require(..)
  , CondExpr(..)
  , Op(..)
  , Constraint(..)
  , ConstraintExpr(..)
  , ConstraintExprPrim(..)
  , ContextIndex(..)
  , COp(..)
  , CEqOp(..)
  , RoleMlsOp(..)
  , SidContext(..)
  , PortContext(..)
  , NetInterfaceContext(..)
  , NodeContext(..)
  , Protocol(..)
  , FileSystemUse(..)
  , GenFileSystemContext(..)
  , FilePath(..)
  , FileType(..)
  , IPV4Address(..)
  , IPV6Address(..)
  , IPAddressMask(..)
  , SecurityContext(..)
  , Transition(..)
  , SourceTarget(..)
  , AllowDeny(..)
  , Self(..)
  , NeverAllow(..)
  , SignedId(..)
  , signedId2Id
  , Sign(..)
  , Permissions(..)
  , StarTilde(..)
  , User(..)
  , Identifier(..)
  , IsIdentifier(..)
  , mkId
  , withIdString
  , ClassId(..)
  , CommonId(..)
  , PermissionId(..)
  , TypeId(..)
  , AttributeId(..)
  , TypeOrAttributeId(..)
  , UserId(..)
  , RoleId(..)
  , NetInterfaceId(..)
  , FileSystemId(..)
  , Sid(..)
  , BoolId(..)
  ) where

import Data.Tree(Tree)
import Data.Word(Word8, Word16)
import Data.Array.IArray(listArray, elems)
import Data.Array.Unboxed(UArray)
import Data.NonEmptyList(NonEmptyList)
import Data.Generics(Typeable, Data(..))
import Prelude hiding (FilePath)
import Text.Happy.ParserMonad(Pos, noPos)

data Identifier = I !Pos (UArray Int Char)
  deriving Typeable

instance Data Identifier where
  gfoldl = undefined
  gunfold = undefined
  toConstr = undefined
  dataTypeOf = undefined

-- | Ignore the position information when comparing Identifiers.
instance Eq Identifier where
  I _ s1 == I _ s2 = s1 == s2

instance Ord Identifier where
  I _ s1 `compare` I _ s2 = s1 `compare` s2

instance Show Identifier where
  show = show . idString

instance Read Identifier where
  readsPrec i s = [(mkId a,b) | (a,b) <- readsPrec i s]

class (Show a, Ord a) => IsIdentifier a where
  idString :: a -> String
  mkId'    :: Pos -> String -> a
  fromId   :: Identifier -> a
  toId     :: a -> Identifier
  pos      :: a -> Pos

mkId :: IsIdentifier i => String -> i
mkId = mkId' noPos

withIdString :: IsIdentifier i => (String -> String) -> i -> i
withIdString f i = mkId' (pos i) (f (idString i))

instance IsIdentifier Identifier where
  idString (I _ s) = elems s
  mkId' p s = I p $ listArray (1,length s) s
  fromId i = i
  toId i = i
  pos (I p _) = p

newtype ClassId = ClassId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype CommonId = CommonId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype PermissionId = PermissionId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype TypeId = TypeId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype AttributeId = AttributeId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype TypeOrAttributeId = TypeOrAttributeId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype Sid = Sid Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype BoolId = BoolId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype UserId = UserId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype RoleId = RoleId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype NetInterfaceId = NetInterfaceId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

newtype FileSystemId = FileSystemId Identifier
  deriving (Typeable, Data, Eq, Read, Show, Ord, IsIdentifier)

data Policy = Policy{ classes :: NonEmptyList ClassId
                    , initialSids :: NonEmptyList Sid
                    , commonPerms :: [CommonPerm]
                    , avPerms     :: NonEmptyList AvPerm
                    , teRbacs :: [TeRbac]
                    , users   :: NonEmptyList User
                    , constraints :: [Constraint]
                    , sidContexts :: NonEmptyList (SidContext SecurityContext)
                    , fileSystemUses :: [FileSystemUse SecurityContext]
                    , genFileSystemContexts :: [GenFileSystemContext SecurityContext]
                    , portContexts :: [PortContext SecurityContext]
                    , netInterfaceContexts :: [NetInterfaceContext SecurityContext]
                    , nodeContexts :: [NodeContext SecurityContext]
                    }
  deriving (Typeable, Data, Eq, Read, Show)

data CommonPerm = CommonPerm CommonId (NonEmptyList PermissionId)
  deriving (Typeable, Data, Eq, Read, Show)

data AvPerm = AvPermClass ClassId (Either (NonEmptyList PermissionId) (CommonId, [PermissionId]))
  deriving (Typeable, Data, Eq, Read, Show)

data TeRbac =
   Attribute AttributeId
 | Type TypeId [TypeId] [AttributeId]
 | TypeAlias TypeId (NonEmptyList TypeId)
 | TypeAttribute TypeId (NonEmptyList AttributeId)
 | BoolDef BoolId Bool
-- -- | RangeTrans SourceTarget MlsRange
 | TeNeverAllow (SourceTarget (NeverAllow TypeOrAttributeId) 
                              (NeverAllow Self))
                Permissions
 | Role RoleId [SignedId TypeOrAttributeId]
 | Dominance (NonEmptyList (Tree RoleId))
 | RoleTransition (NonEmptyList RoleId) (NonEmptyList (SignedId TypeOrAttributeId)) RoleId
 | RoleAllow (NonEmptyList RoleId) (NonEmptyList RoleId)
 | CondStmt CondExpr [RequireStmt] [RequireStmt]
 | Stmt Stmt
 | Optional AvRuleBlock (Maybe AvRuleBlock)
  deriving (Typeable, Data, Eq, Read, Show)

data Stmt = 
   Transition Transition (SourceTarget (NonEmptyList (SignedId TypeOrAttributeId))
                                       (NonEmptyList (SignedId TypeOrAttributeId))) 
                         TypeId
 | TeAvTab AllowDeny (SourceTarget (NonEmptyList (SignedId TypeOrAttributeId))
                                   (NonEmptyList (SignedId Self)))
                     Permissions
  deriving (Typeable, Data, Eq, Read, Show)

data AvRuleBlock =
   AvRuleBlock [AvRule] [User]
  deriving (Typeable, Data, Eq, Read, Show)

data AvRule =
   TeRbac TeRbac
 | AvRuleRequire (NonEmptyList Require)
  deriving (Typeable, Data, Eq, Read, Show)

data RequireStmt =
   RequireStmt Stmt
 | Require (NonEmptyList Require)
  deriving (Typeable, Data, Eq, Read, Show)

data Require =
   RequireClass ClassId (NonEmptyList PermissionId)
 | RequireRole (NonEmptyList RoleId)
 | RequireType (NonEmptyList TypeId)
 | RequireAttribute (NonEmptyList AttributeId)
 | RequireUser (NonEmptyList UserId)
 | RequireBool (NonEmptyList BoolId)
  deriving (Typeable, Data, Eq, Read, Show)
  
data CondExpr =
   Not CondExpr
 | Op CondExpr Op CondExpr
 | Var BoolId
  deriving (Typeable, Data, Eq, Ord, Read, Show)

data Op = And | Or | Xor | Equals | Notequal
  deriving (Typeable, Data, Eq, Ord, Read, Show, Enum)

data Constraint =
   Constrain (NonEmptyList ClassId) (NonEmptyList PermissionId) ConstraintExpr
 | ValidateTrans (NonEmptyList ClassId) ConstraintExpr
  deriving (Typeable, Data, Eq, Read, Show)

data ConstraintExpr =
   ConstraintExprPrim ConstraintExprPrim
 | CNot ConstraintExpr
 | COp ConstraintExpr COp ConstraintExpr
  deriving (Typeable, Data, Eq, Read, Show)

data ConstraintExprPrim =
   CUsers CEqOp
 | CRoles RoleMlsOp
 | CTypes CEqOp
 | CUserSet ContextIndex CEqOp (NonEmptyList UserId)
 | CRoleSet ContextIndex CEqOp (NonEmptyList RoleId)
 | CTypeSet ContextIndex CEqOp (NonEmptyList TypeOrAttributeId)
  deriving (Typeable, Data, Eq, Read, Show)

data ContextIndex = C1 | C2 | C3
  deriving (Typeable, Data, Eq, Read, Show, Enum)

data COp = CAnd | COr
  deriving (Typeable, Data, Eq, Read, Show, Enum)

data CEqOp = CEquals | CNotequal
  deriving (Typeable, Data, Eq, Read, Show, Enum)

data RoleMlsOp = 
   CEqOp CEqOp
 | Dom
 | DomBy
 | InComp
  deriving (Typeable, Data, Eq, Read, Show)

data SidContext s = SidContext Sid s
  deriving (Typeable, Data, Eq, Read, Show)

data PortContext s = PortContext Protocol Word16 Word16 s
  deriving (Typeable, Data, Eq, Read, Show)

data NetInterfaceContext s = NetInterfaceContext NetInterfaceId s s 
  deriving (Typeable, Data, Eq, Read, Show)

-- TODO Capture type relationship between address and mask
data NodeContext s = NodeContext IPAddressMask s
  deriving (Typeable, Data, Eq, Read, Show)

data SecurityContext = SecurityContext UserId RoleId TypeId
  deriving (Typeable, Data, Eq, Read, Show)

data Protocol = Tcp | Udp
  deriving (Typeable, Data, Eq, Read, Show, Enum)

data FileSystemUse s =
   FSUseXattr FileSystemId s
 | FSUseTask FileSystemId s
 | FSUseTrans FileSystemId s
  deriving (Typeable, Data, Eq, Read, Show)

data GenFileSystemContext s = GenFSCon FileSystemId FilePath (Maybe FileType) s
  deriving (Typeable, Data, Eq, Read, Show)

data FilePath = FilePath String
  deriving (Typeable, Data, Eq, Read, Show)

data FileType = BlockFile | CharacterFile | DirectoryFile | FifoFile | LinkFile | SocketFile | PlainFile
  deriving (Typeable, Data, Eq, Read, Show, Enum)

data IPV4Address = IPV4Address Word8 Word8 Word8 Word8
  deriving (Typeable, Data, Eq, Read, Show)
data IPV6Address = IPV6Address Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16 
  deriving (Typeable, Data, Eq, Read, Show)

data IPAddressMask = 
   IPV4AddrMask IPV4Address IPV4Address
 | IPV6AddrMask IPV6Address IPV6Address
  deriving (Typeable, Data, Eq, Read, Show)

data Transition = 
   TypeTransition
 | TypeMember
 | TypeChange
  deriving (Typeable, Data, Eq, Read, Show, Enum)

data SourceTarget st tt = SourceTarget{ sourceTypes :: st
                                      , targetTypes :: tt
                                      , targetClasses :: (NonEmptyList ClassId)
                                      }
  deriving (Typeable, Data, Eq, Read, Show)

data AllowDeny = 
   Allow
 | AuditAllow
 | AuditDeny
 | DontAudit
  deriving (Typeable, Data, Eq, Read, Show, Enum)

{-
data MlsRange = MlsRange Level Level
  deriving (Typeable, Data, Eq, Read, Show)

newtype Level = Level Integer
  deriving (Typeable, Data, Eq, Read, Show)
-}

data Self =
   NotSelf TypeOrAttributeId
 | Self
  deriving (Typeable, Data, Eq, Read, Show)

data NeverAllow t =
   NeverAllow (NonEmptyList (SignedId t))
 | NAStarTilde (StarTilde (SignedId t))
  deriving (Typeable, Data, Eq, Read, Show)

instance Functor NeverAllow where
  fmap f (NeverAllow l) = NeverAllow (fmap (fmap f) l)
  fmap f (NAStarTilde i) = NAStarTilde (fmap (fmap f) i)

data SignedId t = 
   SignedId Sign t
  deriving (Typeable, Data, Eq, Ord, Read, Show)

signedId2Id :: SignedId i -> i
signedId2Id (SignedId _ i) = i

instance Functor SignedId where
  fmap f (SignedId s t) = SignedId s (f t)

data Sign = Negative | Positive
  deriving (Typeable, Data, Eq, Ord, Read, Show, Enum)

data Permissions =
   Permissions (NonEmptyList PermissionId)
 | PStarTilde (StarTilde PermissionId)
  deriving (Typeable, Data, Eq, Read, Show)

data StarTilde i = 
   Star
 | Tilde (NonEmptyList i)
  deriving (Typeable, Data, Eq, Read, Show)

instance Functor StarTilde where
  fmap _ Star = Star
  fmap f (Tilde l) = Tilde (fmap f l)

data User = User UserId (NonEmptyList RoleId)
  deriving (Typeable, Data, Eq, Read, Show)
