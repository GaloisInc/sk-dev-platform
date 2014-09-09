{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

-- | Mangle identifier names
module Lobster.SELinux.Mangle where

import Lobster.Abs
import Lobster.Lex (eitherResIdent, Tok(TL))
import Data.Generics(Typeable, Data, everywhere, mkT)

mangle :: Policy -> Policy
mangle = everywhere (mkT mangleLIdent)

mangleLIdent :: LIdent -> LIdent
mangleLIdent (LIdent s) = LIdent (mangleString s)

mangleString :: String -> String
mangleString s | keyword s || lastIsMangleChar s = s++"_"
               | otherwise                       = s

demangleString :: String -> String
demangleString s | lastIsMangleChar s = init s
                 | otherwise          = s

lastIsMangleChar :: String -> Bool
lastIsMangleChar s = take 1 (reverse s) == "_"

-- Caution: this depends on bnfc internals:
keyword :: String -> Bool
keyword s = case eitherResIdent TL s of TL _ -> False; _ -> True

deriving instance Typeable Policy
deriving instance Typeable Statement
deriving instance Typeable PortDeclarationType
deriving instance Typeable PortDeclarationConnection
deriving instance Typeable Expression
deriving instance Typeable QualName
deriving instance Typeable Name
deriving instance Typeable PortTypeConstraint
deriving instance Typeable NoneExpression
deriving instance Typeable Position
deriving instance Typeable Direction
deriving instance Typeable Connection
deriving instance Typeable PortId
deriving instance Typeable FlowId
deriving instance Typeable Identifier
deriving instance Typeable ClassId
deriving instance Typeable LIdent
deriving instance Typeable UIdent
deriving instance Typeable ConnRE
deriving instance Typeable FlowPred
deriving instance Typeable PortRE
deriving instance Typeable FlowRE
deriving instance Typeable DomainSpec

deriving instance Data Policy
deriving instance Data Statement
deriving instance Data PortDeclarationType
deriving instance Data PortDeclarationConnection
deriving instance Data Expression
deriving instance Data QualName
deriving instance Data Name
deriving instance Data PortTypeConstraint
deriving instance Data NoneExpression
deriving instance Data Position
deriving instance Data Direction
deriving instance Data Connection
deriving instance Data PortId
deriving instance Data FlowId
deriving instance Data Identifier
deriving instance Data ClassId
deriving instance Data LIdent
deriving instance Data UIdent
deriving instance Data ConnRE
deriving instance Data DomainSpec
deriving instance Data PortRE
deriving instance Data FlowPred
deriving instance Data FlowRE
