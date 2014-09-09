{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

-- | Mangle identifier names 
module SCD.Lobster.Gen.CoreSyn.Mangle where

import SCD.Lobster.Gen.CoreSyn
import SCD.Lobster.Mangle(mangleString)
import Data.Generics(Typeable, Data, everywhere, mkT)

mangle :: [Decl] -> [Decl]
mangle = everywhere (mkT mangleName)

mangleName :: Name -> Name
mangleName (Name n) = Name (mangleString n)

deriving instance Typeable Decl
deriving instance Typeable DomPort
deriving instance Typeable Dir
deriving instance Typeable PortConstraint
deriving instance Typeable Name

deriving instance Data Decl
deriving instance Data DomPort
deriving instance Data Dir
deriving instance Data PortConstraint
deriving instance Data Name

