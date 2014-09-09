{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module SCD.Shrimp.RefPolicy.Modules.Services.Dbus{-(Dbus, UserDbus, dbus, systemBusClientTemplate,
  userBusClientTemplate, readConfig, connectSystemBus, sendSystemBus,
  sendUser) -}where

import SCD.Shrimp.Shrimp(Template, Interface(..), interface, Role, Domain(..), newType, Rule(..), IsType(..), Type(..))

import SCD.SELinux.Syntax(Stmt(..), SourceTarget(..),
  IsIdentifier(..), mkId, AllowDeny(..), Self(..), ClassId,
  PermissionId, TypeOrAttributeId, Transition(..), SignedId(..),
  Sign(..), StarTilde(..), Permissions(..))

import Prelude hiding (init)
import Data.NonEmptyList(NonEmptyList, fromList)

data SystemDbus = SystemDbus
  { dbus :: Role -> Domain -> Template Dbus
  }

data Dbus = Dbus
  { systemBusClientTemplate :: Domain -> Template ()
  , userBusClientTemplate   :: Domain -> Template UserDbus
  , readConfig              :: Domain -> Interface
  , connectSystemBus        :: Domain -> Interface
  , sendSystemBus           :: Domain -> Interface
  }

data UserDbus = UserDbus
  { sendUser :: Domain -> Interface
  }

-- these should be defined in other modules, e.g. we should really say
-- import SCD.Shrimp.ReferencePolicy.System.Init(Init)
data Init         = SystemInit
data Files        = KernelFiles
data Kernel       = KernelKernel
data Devices      = KernelDevices
data Filesystem   = KernelFilesystem
data Selinux      = KernelSelinux
data Terminal     = KernelTerminal
data Authlogin    = SystemAuthlogin
data Corecommands = KernelCorecommands
data Kerneldomain = KernelDomain
data Libraries    = SystemLibraries
data Logging      = SystemLogging
data Miscfiles    = SystemMiscfiles
data Seutil       = SystemSelinuxutil
data Userdomain   = SystemUserdomain
data Sysnetwork   = SystemSysnetwork
data Bind         = ServicesBind
data Udev         = SystemUdev

systemDbus :: 
  Init -> 
  Files -> 
{-
  Kernel -> 
  Devices -> 
  Filesystem -> 
  Selinux -> 
  Terminal -> 
  Authlogin -> 
  Corecommands -> 
  Kerneldomain -> 
  Libraries -> 
  Logging -> 
  Miscfiles -> 
  Seutil ->
  Userdomain ->
  Sysnetwork -> 
  Udev ->
-}
  Template SystemDbus
systemDbus init files {-kernel devices filesystem selinux terminal authlogin corecommands kerneldomain libraries logging miscfiles seutil userdomain sysnetwork udev-} = do
  d <- newInitSystemDomain init ["system_dbusd"]
  etc_t <- newFileType files ["dbusd_etc"]
--  tmp_t <- newTmpFile files ["system_dbusd_tmp"]
  var_run_t <- newPidFile files ["system_dbusd_var_run"]
  interface d [ allow [self] [capability] [dac_override, setgid, setpcap, setuid]
              , readFiles files etc_t etc_t
              , readLnkFiles files etc_t etc_t
                -- ...
              , filesPidFiletrans files var_run_t file
                -- ...
              ]
  return SystemDbus
    { dbus = \r d1 -> do
        t <- newDomain [str d1, "dbusd"]
        t2 <- newDomain [str d1, "dbusd_sub"]
--        tmp_t <- newTmpFile files [str d', "dbusd_tmp"]
        interface d1 [ allow [t] [unix_stream_socket] [connectto]
                     , typeChange [t] [dbus_class] t2
                     , domainEntryFile d
                     , role r
                     , allow [self] [process] [getattr, sigkill, signal]
                  ]
        return Dbus
          { systemBusClientTemplate = undefined
          , userBusClientTemplate = \_d2 -> do 
              return UserDbus
                { sendUser = undefined
                }
          , readConfig = undefined
          , connectSystemBus = undefined
          , sendSystemBus = undefined
          }
    }

newInitSystemDomain :: Init -> [String] -> Template Domain
newInitSystemDomain _init l = newType l

newtype File = File Type
  deriving (Eq, Read, Show, Ord, IsIdentifier, IsType)

newFileType :: Files -> [String] -> Template File
newFileType _files l = newType l

newTmpFile :: Files -> [String] -> Template File
newTmpFile = newFileType

newPidFile :: Files -> [String] -> Template File
newPidFile = newFileType

allow :: (SelfOr t, StarOr p) => [t] -> [Class] -> p -> Domain -> Interface
allow sts cs ps d = Interface [StmtRule $ TeAvTab Allow 
                                   SourceTarget{ sourceTypes = fromList [SignedId Positive (toa d)]
                                               , targetTypes = fmap (SignedId Positive . selfOr) (fromList sts)
                                               , targetClasses = fromList cs
                                               } (starOr ps)]

class SelfOr t where 
  selfOr :: t -> Self

instance SelfOr OnlySelf where
  selfOr _ = Self

instance SelfOr Type where
  selfOr = NotSelf . toa

instance SelfOr Domain where
  selfOr = selfOr . domainType

data OnlySelf = OnlySelf
self :: OnlySelf
self = OnlySelf

class StarOr p where
  starOr :: p -> Permissions

instance StarOr (NonEmptyList PermissionId) where
  starOr ps = Permissions ps

instance StarOr [PermissionId] where
  starOr = Permissions . fromList

data OnlyStar = OnlyStar
star :: OnlyStar
star = OnlyStar
instance StarOr OnlyStar where
  starOr _ = PStarTilde Star

type Class = ClassId

capability :: Class
capability = mkId "capability"

type Permission = PermissionId

dac_override, setgid, setpcap, setuid :: Permission
dac_override = mkId "dac_override"
setgid = mkId "setgid"
setpcap = mkId "setpcap"
setuid = mkId "setuid"

readFiles :: Files -> File -> File -> Domain -> Interface
readFiles _ _ _ _ = Interface []

readLnkFiles :: Files -> File -> File -> Domain -> Interface
readLnkFiles _ _ _ _ = Interface []

filesPidFiletrans :: Files -> File -> Class -> Domain -> Interface
filesPidFiletrans _ _ _ _ = Interface []

file :: Class
file = mkId "file"

newDomain :: [String] -> Template Domain
newDomain = newType

str :: Domain -> String
str = idString

unix_stream_socket :: Class
unix_stream_socket = mkId "unix_stream_socket"

connectto :: Permission
connectto = mkId "connectto"

typeChange :: IsType t => [t] -> [Class] -> t -> Domain -> Interface
typeChange ts cs t d = Interface [StmtRule $ Transition TypeChange 
                                           SourceTarget{ sourceTypes = fromList [SignedId Positive (toa d)]
                                                       , targetTypes = fmap (SignedId Positive . toa) (fromList ts)
                                                       , targetClasses = fromList cs
                                                       } (typeId (toType t))]

toa :: IsType t => t -> TypeOrAttributeId
toa = fromId . toId . typeId . toType

dbus_class :: Class
dbus_class = mkId "dbus"

domainEntryFile :: Domain -> Domain -> Interface
domainEntryFile _ _ = Interface []

role :: Role -> Domain -> Interface
role r d = Interface [RoleRule r d]

process :: Class
process = mkId "process"

getattr, sigkill, signal :: Permission
getattr = mkId "getattr"
sigkill = mkId "setattr"
signal = mkId "signal"

