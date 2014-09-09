{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      :  $Header$
Description :  Tests the SELinux policy parser
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Tests the SELinux policy parser.
-}

module SCD.SELinux.Test.Parser where


import Test.QuickCheck hiding ( NonEmptyList, Positive )

import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Providers.API ( Test )

import SCD.SELinux.Syntax( Policy(..), CommonPerm(..), AvPerm(..),
      TeRbac(..), Stmt(..), AvRuleBlock(..), AvRule(..),
      RequireStmt(..), Require(..), CondExpr(..), Op(..),
      Constraint(..), ConstraintExpr(..), ConstraintExprPrim(..),
      ContextIndex(..), COp(..), CEqOp(..), RoleMlsOp(..),
      SidContext(..), PortContext(..), NetInterfaceContext(..),
      NodeContext(..), Protocol(..), FileSystemUse(..),
      GenFileSystemContext(..), FilePath(..), FileType(..),
      IPV4Address(..), IPV6Address(..), IPAddressMask(..),
      SecurityContext(..), Transition(..), SourceTarget(..),
      AllowDeny(..), Self(..), NeverAllow(..), SignedId(..), Sign(..),
      User(..), Permissions(..), StarTilde(..), IsIdentifier(..),
      mkId, ClassId, CommonId, PermissionId, TypeId, AttributeId,
      TypeOrAttributeId, Sid, BoolId, UserId, RoleId, NetInterfaceId,
      FileSystemId)

import SCD.SELinux.PrettyPrint   (Pp(..))
import SCD.SELinux.Parser        (parsePolicy)
import Text.PrettyPrint.HughesPJ (render)
import Control.Monad             (ap)
import Data.Bits                 (Bits(..))
import Data.Tree                 (Tree(..))
import Data.Word                 (Word16)
import Data.NonEmptyList         (NonEmptyList, fromList)
import Data.Foldable             (toList)
import Prelude hiding (FilePath)
import qualified System.FilePath as FilePath

atleastone :: Arbitrary a => Gen (NonEmptyList a)
atleastone = arbitrary

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = return ((fromList .) . (:)) `ap` arbitrary
                                          `ap` arbitrary

instance CoArbitrary a => CoArbitrary (NonEmptyList a) where
  coarbitrary = coarbitrary . toList

arbitraryI :: IsIdentifier i => Gen i
arbitraryI = (mkId . ("v"++) . show . (`mod`5))
             `fmap` (arbitrary :: Gen Word16)

coarbitraryI :: IsIdentifier i => i -> Gen a -> Gen a
coarbitraryI i =
  let s = idString i in
  case s of
    'v':s' ->
        variant (0::Integer) .
        case reads s' of
          [(w, "")] -> variant (0::Integer) . coarbitrary (w::Word16)
          _ -> variant (length s' + 1)
    _ -> variant (length s + 1)

instance Arbitrary ClassId where
  arbitrary = arbitraryI

instance CoArbitrary ClassId where
  coarbitrary = coarbitraryI

instance Arbitrary CommonId where
  arbitrary = arbitraryI

instance CoArbitrary CommonId where
  coarbitrary = coarbitraryI

instance Arbitrary PermissionId where
  arbitrary = arbitraryI

instance CoArbitrary PermissionId where
  coarbitrary = coarbitraryI

instance Arbitrary TypeId where
  arbitrary = arbitraryI

instance CoArbitrary TypeId where
  coarbitrary = coarbitraryI

instance Arbitrary AttributeId where
  arbitrary = arbitraryI

instance CoArbitrary AttributeId where
  coarbitrary = coarbitraryI

instance Arbitrary TypeOrAttributeId where
  arbitrary = arbitraryI

instance CoArbitrary TypeOrAttributeId where
  coarbitrary = coarbitraryI

instance Arbitrary Sid where
  arbitrary = arbitraryI

instance CoArbitrary Sid where
  coarbitrary = coarbitraryI

instance Arbitrary BoolId where
  arbitrary = arbitraryI

instance CoArbitrary BoolId where
  coarbitrary = coarbitraryI

instance Arbitrary UserId where
  arbitrary = arbitraryI

instance CoArbitrary UserId where
  coarbitrary = coarbitraryI

instance Arbitrary RoleId where
  arbitrary = arbitraryI

instance CoArbitrary RoleId where
  coarbitrary = coarbitraryI

instance Arbitrary NetInterfaceId where
  arbitrary = arbitraryI

instance CoArbitrary NetInterfaceId where
  coarbitrary = coarbitraryI

instance Arbitrary FileSystemId where
  arbitrary = arbitraryI

instance CoArbitrary FileSystemId where
  coarbitrary = coarbitraryI

instance Arbitrary Policy where
  arbitrary = 
    do c <- atleastone
       i <- atleastone
       cp <- arbitrary
       a <- atleastone
       t <- arbitraryList (arbitraryTeRbac 2)
       u <- atleastone
       constrs <- arbitrary
       scs <- atleastone
       fsu <- arbitrary
       fsc <- arbitrary
       pc <- arbitrary
       nic <- arbitrary
       nodes <- arbitrary
       return Policy{ classes = c
                    , initialSids = i
                    , commonPerms = cp
                    , avPerms = a
                    , teRbacs = t
                    , users   = u
                    , constraints = constrs
                    , sidContexts = scs
                    , fileSystemUses = fsu
                    , genFileSystemContexts = fsc
                    , portContexts = pc
                    , netInterfaceContexts = nic
                    , nodeContexts = nodes
                    }

instance CoArbitrary Policy where
  coarbitrary (Policy {classes = c
                      , initialSids = i
                      , commonPerms = cp
                      , avPerms = a
                      , teRbacs = t
                      , users   = u
                      , constraints = constrs
                      , sidContexts = scs
                      , fileSystemUses = fsu
                      , genFileSystemContexts = fsc
                      , portContexts = pc
                      , netInterfaceContexts = nic
                      , nodeContexts = nodes
                      }) =
    coarbitrary c .
    coarbitrary i .
    coarbitrary cp .
    coarbitrary a .
    coarbitrary t .
    coarbitrary u .
    coarbitrary constrs .
    coarbitrary scs .
    coarbitrary fsu .
    coarbitrary fsc .
    coarbitrary pc .
    coarbitrary nic .
    coarbitrary nodes

instance Arbitrary CommonPerm where
  arbitrary = return CommonPerm
         `ap` arbitrary
         `ap` atleastone

instance CoArbitrary CommonPerm where
  coarbitrary (CommonPerm c ps) = coarbitrary c . coarbitrary ps

instance Arbitrary AvPerm where
  arbitrary = oneof [ return AvPermClass `ap` arbitrary `ap` (return Left `ap` atleastone)
                    , return AvPermClass `ap` arbitrary `ap` (return ((Right .) .(,))
                                                            `ap` arbitrary `ap` arbitrary)
                    ]

instance CoArbitrary AvPerm where
  coarbitrary (AvPermClass c d) =
    coarbitrary c . coarbitrary d

arbitraryTeRbac :: Int -> Gen TeRbac
arbitraryTeRbac 0 = oneof nonRecursiveTeRbacs
arbitraryTeRbac n = oneof ((return Optional `ap` arbitraryAvRuleBlock (n - 1) 
                                            `ap` arbitraryMaybe (arbitraryAvRuleBlock (n - 1))) 
                           : nonRecursiveTeRbacs)

nonRecursiveTeRbacs :: [Gen TeRbac]
nonRecursiveTeRbacs =  [ return Attribute      `ap` arbitrary
                       , return Type           `ap` arbitrary `ap` arbitrary `ap` arbitrary
                       , return TypeAlias      `ap` arbitrary `ap` atleastone
                       , return TypeAttribute  `ap` arbitrary `ap` atleastone
                       , return BoolDef        `ap` arbitrary `ap` arbitrary
                       , return TeNeverAllow   `ap` arbitrary `ap` arbitrary
                       , return Role           `ap` arbitrary `ap` arbitrary
                       , return Dominance      `ap` atleastone
                       , return RoleTransition `ap` atleastone `ap` atleastone `ap` arbitrary
                       , return RoleAllow      `ap` atleastone `ap` atleastone
                       , return CondStmt       `ap` arbitrary `ap` arbitrary `ap` arbitrary
                       , return Stmt           `ap` arbitrary
                       ]

instance Arbitrary TeRbac where
  arbitrary = sized arbitraryTeRbac

instance CoArbitrary TeRbac where
  coarbitrary (Attribute i) =
    variant (0::Integer) . coarbitrary i
  coarbitrary (Type ti tis ai) =
    variant (1::Integer) . coarbitrary ti . coarbitrary tis . coarbitrary ai
  coarbitrary (TypeAlias ti tis) =
    variant (2::Integer) . coarbitrary ti . coarbitrary tis
  coarbitrary (TypeAttribute ti ais) =
    variant (3::Integer) . coarbitrary ti . coarbitrary ais
  coarbitrary (BoolDef bi b) =
    variant (4::Integer) . coarbitrary bi . coarbitrary b
-- coarbitrary (RangeTrans st mr) =
--   variant 5 . coarbitrary st . coarbitrary mr
  coarbitrary (TeNeverAllow st p) =
    variant (6::Integer) . coarbitrary st . coarbitrary p
  coarbitrary (Role r sis) =
    variant (7::Integer) . coarbitrary r . coarbitrary sis
  coarbitrary (Dominance f) =
    variant (8::Integer) . coarbitrary f
  coarbitrary (RoleTransition ris sis ri) =
    variant (9::Integer) . coarbitrary ris . coarbitrary sis . coarbitrary ri
  coarbitrary (RoleAllow ris1 ris2) =
    variant (10::Integer) . coarbitrary ris1 . coarbitrary ris2
  coarbitrary (CondStmt c ss1 ss2) =
    variant (11::Integer) . coarbitrary c . coarbitrary ss1 . coarbitrary ss2
  coarbitrary (Stmt s) =
    variant (12::Integer) . coarbitrary s
  coarbitrary (Optional b bm) =
    variant (13::Integer) . coarbitrary b . coarbitrary bm

instance Arbitrary Stmt where
  arbitrary = oneof [ return Transition     `ap` arbitrary `ap` arbitrary `ap` arbitrary
                    , return TeAvTab        `ap` arbitrary `ap` arbitrary `ap` arbitrary
                    ]

instance CoArbitrary Stmt where
  coarbitrary (Transition t st ti) =
    variant (0::Integer) . coarbitrary t . coarbitrary st . coarbitrary ti
  coarbitrary (TeAvTab ad st p) =
    variant (1::Integer) . coarbitrary ad . coarbitrary st . coarbitrary p

arbitraryAvRuleBlock :: Int -> Gen AvRuleBlock
arbitraryAvRuleBlock n = return AvRuleBlock `ap` arbitraryList (arbitraryAvRule n)
                                            `ap` arbitrary

instance Arbitrary AvRuleBlock where
  arbitrary = sized arbitraryAvRuleBlock

instance CoArbitrary AvRuleBlock where
  coarbitrary (AvRuleBlock rs us) = coarbitrary rs . coarbitrary us

arbitraryAvRule :: Int -> Gen AvRule
arbitraryAvRule n = oneof [ return TeRbac `ap` arbitraryTeRbac n
                          , return AvRuleRequire `ap` atleastone
                          ]

instance Arbitrary RequireStmt where
  arbitrary = oneof [ return RequireStmt `ap` arbitrary
                    , return Require `ap` atleastone
                    ]

instance CoArbitrary RequireStmt where
  coarbitrary (RequireStmt t) = variant (0::Integer) . coarbitrary t
  coarbitrary (Require t)     = variant (1::Integer) . coarbitrary t

instance Arbitrary Require where
  arbitrary = oneof [ return RequireClass `ap` arbitrary `ap` atleastone
                    , return RequireRole `ap` atleastone
                    , return RequireType `ap` atleastone
                    , return RequireAttribute `ap` atleastone
                    , return RequireUser `ap` atleastone
                    , return RequireBool `ap` atleastone
                    ]

instance CoArbitrary Require where
  coarbitrary (RequireClass i ps)  = variant (0::Integer) . coarbitrary i . coarbitrary ps
  coarbitrary (RequireRole l)      = variant (1::Integer) . coarbitrary l
  coarbitrary (RequireType l)      = variant (2::Integer) . coarbitrary l
  coarbitrary (RequireAttribute l) = variant (3::Integer) . coarbitrary l
  coarbitrary (RequireUser l)      = variant (4::Integer) . coarbitrary l
  coarbitrary (RequireBool l)      = variant (5::Integer) . coarbitrary l

instance Arbitrary AvRule where
  arbitrary = sized arbitraryAvRule

instance CoArbitrary AvRule where
  coarbitrary (TeRbac t)        = variant (0::Integer) . coarbitrary t
  coarbitrary (AvRuleRequire r) = variant (1::Integer) . coarbitrary r

instance Arbitrary CondExpr where
  arbitrary = sized arb
    where arb 0 = return Var `ap` arbitrary
          arb n = oneof [ return Not `ap` arb (n - 1)
                        , return Op  `ap` arb (n `div` 2) `ap` arbitrary `ap` arb (n `div` 2)
                        , arb 0
                        ]

instance CoArbitrary CondExpr where
  coarbitrary (Not c) =
    variant (0::Integer) . coarbitrary c
  coarbitrary (Op c1 op c2) =
    variant (1::Integer) . coarbitrary c1 . coarbitrary op . coarbitrary c2
  coarbitrary (Var b) =
    variant (2::Integer) . coarbitrary b

instance Arbitrary Op where
  arbitrary = oneof (map return [And, Or, Xor, Equals, Notequal])

instance CoArbitrary Op where
  coarbitrary = variant . fromEnum

instance Arbitrary Constraint where
  arbitrary = oneof [ return Constrain     `ap` atleastone `ap` atleastone `ap` aConstraintExpr aContextIndexConstrain
                    , return ValidateTrans `ap` atleastone `ap` aConstraintExpr aContextIndexValidateTrans
                    ]

instance CoArbitrary Constraint where
  coarbitrary (Constrain cis pis ce) =
    variant (0::Integer) . coarbitrary cis . coarbitrary pis . coConstraintExpr ce
  coarbitrary (ValidateTrans cis ce) =
    variant (1::Integer) . coarbitrary cis . coConstraintExpr ce

aConstraintExpr :: Gen ContextIndex -> Gen ConstraintExpr
aConstraintExpr gc = sized arb
    where arb 0 = return ConstraintExprPrim `ap` aConstraintExprPrim gc
          arb n = oneof [ return CNot `ap` arb (n - 1)
                        , return COp  `ap` arb (n `div` 2) `ap` arbitrary `ap` arb (n `div` 2)
                        , arb 0
                        ]

coConstraintExpr :: ConstraintExpr -> Gen a -> Gen a
coConstraintExpr (ConstraintExprPrim p) =
  variant (0::Integer) . coConstraintExprPrim p
coConstraintExpr (CNot e) =
  variant (1::Integer) . coConstraintExpr e
coConstraintExpr (COp e1 o e2) =
  variant (2::Integer) . coConstraintExpr e1 . coarbitrary o . coConstraintExpr e2

aConstraintExprPrim :: Gen ContextIndex -> Gen ConstraintExprPrim
aConstraintExprPrim gc = oneof [ return CUsers   `ap` arbitrary
                               , return CRoles   `ap` arbitrary
                               , return CTypes   `ap` arbitrary
                               , return CUserSet `ap` gc `ap` arbitrary `ap` atleastone
                               , return CRoleSet `ap` gc `ap` arbitrary `ap` atleastone
                               , return CTypeSet `ap` gc `ap` arbitrary `ap` atleastone
                               ]

coConstraintExprPrim :: ConstraintExprPrim -> Gen a -> Gen a
coConstraintExprPrim (CUsers o) =
  variant (0::Integer) . coarbitrary o
coConstraintExprPrim (CRoles o) =
  variant (1::Integer) . coarbitrary o
coConstraintExprPrim (CTypes o) =
  variant (2::Integer) . coarbitrary o
coConstraintExprPrim (CUserSet ci o uis) =
  variant (3::Integer) . coContextIndex ci . coarbitrary o . coarbitrary uis
coConstraintExprPrim (CRoleSet ci o ris) =
  variant (4::Integer) . coContextIndex ci . coarbitrary o . coarbitrary ris
coConstraintExprPrim (CTypeSet ci o tais) =
  variant (5::Integer) . coContextIndex ci . coarbitrary o . coarbitrary tais

aContextIndexConstrain :: Gen ContextIndex
aContextIndexConstrain = oneof (map return [C1, C2])

aContextIndexValidateTrans :: Gen ContextIndex
aContextIndexValidateTrans = oneof (map return [C1, C2, C3])

coContextIndex :: ContextIndex -> Gen a -> Gen a
coContextIndex = variant . fromEnum

instance Arbitrary COp where
  arbitrary = oneof (map return [CAnd, COr])

instance CoArbitrary COp where
  coarbitrary = variant . fromEnum

instance Arbitrary CEqOp where
  arbitrary = oneof (map return [CEquals, CNotequal])

instance CoArbitrary CEqOp where
  coarbitrary = variant . fromEnum

instance Arbitrary RoleMlsOp where
  arbitrary = oneof [ return CEqOp `ap` arbitrary
                    , return Dom
                    , return DomBy
                    , return InComp
                    ]

instance CoArbitrary RoleMlsOp where
  coarbitrary (CEqOp o) = variant (0::Integer) . coarbitrary o
  coarbitrary Dom = variant (1::Integer)
  coarbitrary DomBy = variant (2::Integer)
  coarbitrary InComp = variant (3::Integer)

instance Arbitrary s => Arbitrary (SidContext s) where
  arbitrary = return SidContext `ap` arbitrary `ap` arbitrary

instance CoArbitrary s => CoArbitrary (SidContext s) where
  coarbitrary (SidContext si sc) = coarbitrary si . coarbitrary sc

instance Arbitrary s => Arbitrary (PortContext s) where
  arbitrary = do i1 <- arbitrary
                 i2 <- arbitrary
                 oneof [ return PortContext `ap` arbitrary 
                                            `ap` return (min i1 i2) 
                                            `ap` return (max i1 i2) 
                                            `ap` arbitrary
                       , return PortContext `ap` arbitrary 
                                            `ap` return i1
                                            `ap` return i1
                                            `ap` arbitrary
                       ]

instance CoArbitrary s => CoArbitrary (PortContext s) where
  coarbitrary (PortContext p w1 w2 sc) =
    coarbitrary p . coarbitrary w1 . coarbitrary w2 . coarbitrary sc

instance Arbitrary s => Arbitrary (NetInterfaceContext s) where
  arbitrary = return NetInterfaceContext `ap` arbitrary `ap` arbitrary `ap` arbitrary

instance CoArbitrary s => CoArbitrary (NetInterfaceContext s) where
  coarbitrary (NetInterfaceContext ni sc1 sc2) =
    coarbitrary ni . coarbitrary sc1 . coarbitrary sc2

instance Arbitrary s => Arbitrary (NodeContext s) where
  arbitrary = return NodeContext `ap` arbitrary `ap` arbitrary

instance CoArbitrary s => CoArbitrary (NodeContext s) where
  coarbitrary (NodeContext am sc) =
    coarbitrary am . coarbitrary sc

instance Arbitrary SecurityContext where
  arbitrary = return SecurityContext `ap` arbitrary `ap` arbitrary `ap` arbitrary

instance CoArbitrary SecurityContext where
  coarbitrary (SecurityContext ui ri ti) =
    coarbitrary ui . coarbitrary ri . coarbitrary ti

instance Arbitrary Protocol where
  arbitrary = oneof [ return Tcp
                    , return Udp
                    ]

instance CoArbitrary Protocol where
  coarbitrary = variant . fromEnum

instance Arbitrary s => Arbitrary (FileSystemUse s) where
  arbitrary = oneof [ return FSUseXattr `ap` arbitrary `ap` arbitrary
                    , return FSUseTask  `ap` arbitrary `ap` arbitrary
                    , return FSUseTrans `ap` arbitrary `ap` arbitrary
                    ]

instance CoArbitrary s => CoArbitrary (FileSystemUse s) where
  coarbitrary (FSUseXattr fsi sc) =
    variant (0::Integer) . coarbitrary fsi . coarbitrary sc
  coarbitrary (FSUseTask fsi sc) =
    variant (1::Integer) . coarbitrary fsi . coarbitrary sc
  coarbitrary (FSUseTrans fsi sc) =
    variant (2::Integer) . coarbitrary fsi . coarbitrary sc

instance Arbitrary s => Arbitrary (GenFileSystemContext s) where
  arbitrary = return GenFSCon `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary

instance CoArbitrary s => CoArbitrary (GenFileSystemContext s) where
  coarbitrary (GenFSCon fsi fp ft sc) =
    coarbitrary fsi . coarbitrary fp . coarbitrary ft . coarbitrary sc

instance Arbitrary FilePath where
  arbitrary = return (FilePath . ("/"++) . show) `ap` (arbitrary :: Gen Word16)

instance CoArbitrary FilePath where
  coarbitrary (FilePath s) =
    case s of
      '/':s' ->
          variant (0::Integer) .
          case reads s' of
            [(w, "")] -> variant (0::Integer) . coarbitrary (w :: Word16)
            _ -> variant (length s' + 1)
      _ -> variant (length s + 1)

instance Arbitrary FileType where
  arbitrary = oneof [ return BlockFile
                    , return CharacterFile
                    , return DirectoryFile
                    , return FifoFile
                    , return LinkFile
                    , return SocketFile
                    , return PlainFile
                    ]

instance CoArbitrary FileType where
  coarbitrary = variant . fromEnum

instance Arbitrary IPAddressMask where
  arbitrary = oneof [ return IPV4AddrMask `ap` arbitrary `ap` arbitrary
                    , return IPV6AddrMask `ap` arbitrary `ap` arbitrary
                    ]

instance CoArbitrary IPAddressMask where
  coarbitrary (IPV4AddrMask a m) =
    variant (0::Integer) . coarbitrary a . coarbitrary m
  coarbitrary (IPV6AddrMask a m) =
    variant (1::Integer) . coarbitrary a . coarbitrary m

instance Arbitrary IPV4Address where
  arbitrary = return IPV4Address `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary

instance CoArbitrary IPV4Address where
  coarbitrary (IPV4Address w1 w2 w3 w4) =
    coarbitrary w1 . coarbitrary w2 . coarbitrary w3 . coarbitrary w4

instance Arbitrary IPV6Address where
  arbitrary = return IPV6Address `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary
                                 `ap` arbitrary

instance CoArbitrary IPV6Address where
  coarbitrary (IPV6Address w1 w2 w3 w4 w5 w6 w7 w8) =
    coarbitrary w1 . coarbitrary w2 . coarbitrary w3 . coarbitrary w4 .
    coarbitrary w5 . coarbitrary w6 . coarbitrary w7 . coarbitrary w8

instance Arbitrary Transition where
  arbitrary = oneof [ return TypeTransition
                    , return TypeMember
                    , return TypeChange
                    ]

instance CoArbitrary Transition where
  coarbitrary = variant . fromEnum

instance (Arbitrary st, Arbitrary tt) => Arbitrary (SourceTarget (NonEmptyList st) (NonEmptyList tt)) where
  arbitrary =
    do st <- atleastone
       tt <- atleastone
       cs <- atleastone
       return SourceTarget{ sourceTypes = st
                          , targetTypes = tt
                          , targetClasses = cs
                          }

instance (CoArbitrary st, CoArbitrary tt) =>
    CoArbitrary (SourceTarget (NonEmptyList st) (NonEmptyList tt)) where
  coarbitrary (SourceTarget {sourceTypes = st,
                             targetTypes = tt,
                             targetClasses = cs}) =
    coarbitrary st . coarbitrary tt . coarbitrary cs

instance Arbitrary (SourceTarget (NeverAllow TypeOrAttributeId) (NeverAllow Self)) where
  arbitrary =
    do st <- arbitrary
       tt <- arbitrary
       cs <- atleastone
       return SourceTarget{ sourceTypes = st
                          , targetTypes = tt
                          , targetClasses = cs
                          }

instance CoArbitrary (SourceTarget (NeverAllow TypeOrAttributeId) (NeverAllow Self)) where
  coarbitrary (SourceTarget {sourceTypes = st,
                             targetTypes = tt,
                             targetClasses = cs}) =
    coarbitrary st . coarbitrary tt . coarbitrary cs

instance Arbitrary AllowDeny where
  arbitrary = oneof [ return Allow
                    , return AuditAllow
                    , return AuditDeny
                    , return DontAudit
                    ]

instance CoArbitrary AllowDeny where
  coarbitrary = variant . fromEnum

instance Arbitrary Self where
  arbitrary = oneof [ return NotSelf `ap` arbitrary
                    , return Self
                    ]

instance CoArbitrary Self where
  coarbitrary (NotSelf t) = variant (0::Integer) . coarbitrary t
  coarbitrary Self        = variant (1::Integer)

instance Arbitrary t => Arbitrary (NeverAllow t) where
  arbitrary = oneof [ return NeverAllow  `ap` atleastone
                    , return NAStarTilde `ap` arbitrary
                    ]

instance CoArbitrary t => CoArbitrary (NeverAllow t) where
  coarbitrary (NeverAllow t) = variant (0::Integer) . coarbitrary t
  coarbitrary (NAStarTilde st) = variant (1::Integer) . coarbitrary st

instance Arbitrary t => Arbitrary (SignedId t) where
  arbitrary = return SignedId `ap` arbitrary `ap` arbitrary

instance CoArbitrary t => CoArbitrary (SignedId t) where
  coarbitrary (SignedId b tai) = coarbitrary b . coarbitrary tai

instance Arbitrary Sign where
  arbitrary = oneof (map return [Negative, Positive])

instance CoArbitrary Sign where
  coarbitrary = variant . fromEnum

instance Arbitrary Permissions where
  arbitrary = oneof [ return Permissions `ap` atleastone
                    , return PStarTilde  `ap` arbitrary
                    ]

instance CoArbitrary Permissions where
  coarbitrary (Permissions pis) = variant (0::Integer) . coarbitrary pis
  coarbitrary (PStarTilde st) = variant (1::Integer) . coarbitrary st

instance Arbitrary t => Arbitrary (StarTilde t) where
  arbitrary = oneof [ return Star
                    , return Tilde `ap` atleastone
                    ]

instance CoArbitrary t => CoArbitrary (StarTilde t) where
  coarbitrary Star = variant (0::Integer)
  coarbitrary (Tilde is) = variant (1::Integer) . coarbitrary is

instance Arbitrary User where
  arbitrary = return User `ap` arbitrary `ap` atleastone

instance CoArbitrary User where
  coarbitrary (User ui ris) = coarbitrary ui . coarbitrary ris

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arb
    where arb 0 = return Node `ap` arbitrary `ap` return []
          arb n = return Node `ap` arbitrary `ap` arbs (n `div` 2)
          arbs 0 = return []
          arbs n = frequency [ (1, return [])
                             , (4, return (:) `ap` arb (n `div` 2) `ap` arbs (n `div` 2))
                             ]

instance CoArbitrary a => CoArbitrary (Tree a) where
  coarbitrary (Node {rootLabel = l,
                     subForest = s}) =
    coarbitrary l . coarbitrary s

-- <rant>if we only could pass in dictionaries ourselves to methods,
-- we wouldn't need these definitions:

arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe g = oneof [ return Nothing
                         , return Just `ap` g
                         ]

arbitraryVector :: Gen a -> Int -> Gen [a]
arbitraryVector g n = sequence [ g | _ <- [1..n] ]

arbitraryList :: Gen a -> Gen [a]
arbitraryList g = sized (\n -> choose (0,n) >>= arbitraryVector g)

-- </rant>

wordToInt :: (Bits a, Integral a) => a -> Int
wordToInt w = if testBit w (bitSize w - 1) then
                2 * fromIntegral (complement w) + 1
              else
                2 * fromIntegral w

-- instance Arbitrary Word8 where
--   arbitrary = return fromInteger `ap` arbitrary

-- instance CoArbitrary Word8 where
--   coarbitrary w = variant (wordToInt w)

-- instance Arbitrary Word16 where
--   arbitrary = return fromInteger `ap` arbitrary

-- instance CoArbitrary Word16 where
--   coarbitrary w = variant (wordToInt w)

testN :: Testable a => Int -> String -> a -> IO ()
testN n s t = putStr (s++": ") >> quickCheckWith stdArgs { maxSuccess=n } t
--testN n s t = putStr (s++": ") >> check defaultConfig{configMaxTest=n} t

newtype ParseTest a = ParseTest a
  deriving Arbitrary

class Parsable a where
  parse :: FilePath.FilePath -> String -> Either String a

instance Parsable Policy where parse = parsePolicy

instance (Pp a, Show a, Parsable a) => Show (ParseTest a) where
  show (ParseTest p) = show p ++ del ++ ps ++ del ++
                       case parse "" ps of
                         Right ps' -> show (ps'::a) ++ del ++ render (pp ps')
                         Left err -> err
        where ps = render (pp p)
              del = "\n-------------\n"

testParser :: (Pp a, Eq a) => (FilePath.FilePath -> String -> Either String a) -> ParseTest a -> Bool
testParser p (ParseTest a) = p "" (render (pp a)) == Right a

tryParsePolicyFile :: String -> IO (Either String Policy)
tryParsePolicyFile filename =
    do s <- readFile filename
       return (parse filename s)

parsePolicyFile :: String -> IO Policy
parsePolicyFile filename =
    do p <- tryParsePolicyFile filename
       case p of
          Left e -> error ("ERROR: couldn't parse " ++ filename ++ ":\n" ++ e)
          Right policy -> return policy

parsePolicyFileTest :: String -> IO Policy
parsePolicyFileTest filename =
    do policy <- parsePolicyFile filename
       putStr ("SUCCESS: parsed " ++ filename ++ ".\n")
       return policy

prop_higherOrder :: (Policy -> Int) -> Policy -> Bool
prop_higherOrder f p = f p == f p


intLog :: Integral a => a -> a
intLog = floor . log . fromIntegral

testCases :: String -> [Test]
testCases filename =   -- mkIdentified . idString was run 100x
                     [  testProperty "mkIdentifier . idString = id" $
                                    \i -> mkId (idString i) == (i :: TypeId)
                     -- parse . render... was run 100x
                     , testProperty "parse . render . ppPolicy = id" $
                                    mapSize intLog (testParser parsePolicy)
                     -- higher-order: was run 25x
                     , testProperty "higher-order" $
                                    mapSize intLog prop_higherOrder
                     , testCase "Parse policy file test" $ do
                                    parsePolicyFile filename
                                    return ()
                     ]

checks :: String -> IO Policy
checks filename =
    do
      putStrLn "\nBegin tests of the SELinux policy parser"
      testN 100 "mkIdentifier . idString = id" $ \i -> mkId (idString i) == (i :: TypeId)
      testN 100 "parse . render . ppPolicy = id" $ testParser parsePolicy
      testN 100 "higher-order" $ \f p -> (f :: Policy -> Int) p == f p
      policy <- parsePolicyFileTest filename
      putStrLn "End tests of the SELinux policy parser"
      return policy

instance Show (a->b) where
  show = undefined

parseFile :: String -> IO Policy
parseFile f =
  do s <- readFile f
     either fail return (parse f s)
