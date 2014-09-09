{-# LANGUAGE FlexibleInstances, StandaloneDeriving, 
    GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -cpp #-}
{- |
Module      :  $Header$
Description :  Pretty-printing of SELinux policies
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Pretty-printing of SELinux policies.
-}
module SCD.SELinux.PrettyPrint(Pp(..), IsAtom, ppAlias,
  ppOptAttrs, prettyPrint, ppMaybeFileType, ppListTeRbac) where

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
      User(..), Permissions(..), StarTilde(..), Identifier,
      IsIdentifier(..), ClassId(..), CommonId(..), PermissionId(..),
      TypeId(..), AttributeId(..), TypeOrAttributeId(..), Sid(..),
      BoolId(..), UserId(..), RoleId(..), NetInterfaceId(..),
      FileSystemId(..))

import Text.PrettyPrint.HughesPJ(Doc, text, empty, ($+$), (<+>), (<>),
  semi, comma, colon, punctuate, sep, parens, hcat, render)

import Text.PrettyPrint.Pp(Pp(..), above, pbraces, pbracesH,
  star, minus, tilde, period, sepWithCommas)


import Prelude hiding (FilePath)
import Data.Tree(Tree(..), Forest)
import Data.Word(Word8, Word16)
import Data.NonEmptyList(NonEmptyList)
import Data.Foldable(Foldable, toList)
import Numeric(showHex)

prettyPrint :: Pp a => a -> String
prettyPrint = render . pp

instance Pp Policy where
  pp p = above (fmap ppClass (classes p))
     $+$ above (fmap ppInitialSids (initialSids p))
     $+$ above (commonPerms p)
     $+$ above (avPerms p)
     $+$ above (ppTeRbacs (teRbacs p))
     $+$ above (users p)
     $+$ above (constraints p)
     $+$ above (sidContexts p)
     $+$ above (fileSystemUses p)
     $+$ above (genFileSystemContexts p)
     $+$ above (portContexts p)
     $+$ above (netInterfaceContexts p)
     $+$ above (nodeContexts p)

ppClass :: ClassId -> Doc
ppClass c = text "class" <+> pp c

ppInitialSids :: Sid -> Doc
ppInitialSids i = text "sid" <+> pp i

ppTeRbacs :: [TeRbac] -> [Doc]
ppTeRbacs [] = [semi]
ppTeRbacs l = map pp l

ppListTeRbac :: [TeRbac] -> Doc
ppListTeRbac = above . ppTeRbacs

instance Pp CommonPerm where
  pp (CommonPerm i l) = text "common" <+> pp i $+$ pbraces l

instance Pp AvPerm where
  pp (AvPermClass c inh) = text "class" <+> pp c
                       $+$ either ppPermissions
                                  (\(i,l) -> text "inherits" <+> pp i $+$ ppPermissions l)
                                  inh

ppPermissions :: Foldable l => l PermissionId -> Doc
ppPermissions = ppPermissions' . toList where
  ppPermissions' [] = empty
  ppPermissions' l  = pbraces l

instance Pp TeRbac where
  pp (Attribute i) = text "attribute" <+> pp i <> semi
  pp (Type t als ats) = text "type" <+> pp t <+> ppAlias als <> ppOptAttrs ats <> semi
  pp (TypeAlias t a) = text "typealias" <+> pp t <+> ppAlias a <> semi
  pp (TypeAttribute t l) = text "typeattribute" <+> pp t <+> sepWithCommas l <> semi
  pp (BoolDef i b) = text "bool" <+> pp i
                 <+> pp b <> semi
  pp (TeNeverAllow st ps) = text "neverallow" <+> pp st <+> pp ps <> semi
  pp (Role r []) = text "role" <+> pp r <> semi
  pp (Role r ts) = text "role" <+> pp r <+> text "types" <+> pp ts <> semi
  pp (Dominance d) = text "dominance" <+> ppDominance (toList d)
  pp (RoleTransition rs ts r) = text "role_transition" <+> pp rs
                            <+> pp ts <+> pp r <> semi
  pp (RoleAllow r1 r2) = text "allow" <+> pp r1 <+> pp r2 <> semi
  pp (CondStmt c s1 s2) = text "if" <+> parens (pp c) $+$ pbraces s1
                      $+$ if null s2 then empty else text "else" $+$ pbraces s2
  pp (Stmt s) = pp s
  pp (Optional t me) = text "optional" 
                  $+$ pbraces (ppAvRuleBlock t) 
                  $+$ ppElse me
   where ppElse Nothing  = empty
         ppElse (Just e) = text "else" $+$ pbraces (ppAvRuleBlock e)

instance Pp Bool where
  pp b = text (if b then "true" else "false")

instance Pp Stmt where
  pp (Transition tr st t) = pp tr <+> pp st <+> pp t <> semi
  pp (TeAvTab a st ps) = pp a <+> pp st <+> pp ps <> semi

ppAvRuleBlock :: AvRuleBlock -> [Doc]
ppAvRuleBlock (AvRuleBlock rs us) = (if null rs then [semi] else map pp rs) 
                                ++ map pp us

ppRequire :: Foldable l => l Require -> Doc
ppRequire rs = text "require" $+$ pbraces rs

instance Pp Require where
  pp (RequireClass i ps) = text "class" <+> pp i <+> pp ps <> semi
  pp (RequireRole      l) = text "role"      <+> sepWithCommas l <> semi
  pp (RequireType      l) = text "type"      <+> sepWithCommas l <> semi
  pp (RequireAttribute l) = text "attribute" <+> sepWithCommas l <> semi
  pp (RequireUser      l) = text "user"      <+> sepWithCommas l <> semi
  pp (RequireBool      l) = text "bool"      <+> sepWithCommas l <> semi

instance Pp AvRule where
  pp (TeRbac s) = pp s
  pp (AvRuleRequire r) = ppRequire r

instance Pp RequireStmt where
  pp (RequireStmt s) = pp s
  pp (Require r) = ppRequire r

instance Pp CondExpr where
  pp = ppCondExpr 0

ppCondExpr :: Int -> CondExpr -> Doc
ppCondExpr l (Not e) = mparen l prioNot (text "not" <+> ppCondExpr prioNot e)
ppCondExpr l (Op e1 o e2) = mparen l (prio o) (ppCondExpr (prio o) e1
                                  <+> pp o <+> ppCondExpr (prio o+1) e2)
ppCondExpr _ (Var v) = pp v

instance Pp Op where
  pp And      = text "&&"
  pp Or       = text "||"
  pp Xor      = text "^"
  pp Equals   = text "=="
  pp Notequal = text "!="

prioNot :: Int
prioNot = 4

-- prio should reflect the associativity rules in Parser.y.
prio :: Op -> Int
prio Or = 1
prio Xor = 2
prio And = 3
--prio Not = prioNot = 4
prio Equals = 5
prio Notequal = 5

mparen :: Int -> Int -> Doc -> Doc
mparen o i d | i < o     = parens d
             | otherwise = d

ppAlias :: Foldable l => l TypeId -> Doc
ppAlias = ppAlias' . toList where
  ppAlias' [] = empty
  ppAlias' ts = text "alias" <+> pp ts

ppOptAttrs :: [AttributeId] -> Doc
ppOptAttrs [] = empty
ppOptAttrs l  = comma <+> sepWithCommas l

instance Pp Constraint where
  pp (Constrain c p e)   = text "constrain" <+> pp c <+> pp p <+> pp e <> semi
  pp (ValidateTrans c e) = text "validatetrans" <+> pp c <+> pp e <> semi

instance Pp ConstraintExpr where
  pp = ppConstraintExpr 0

ppConstraintExpr :: Int -> ConstraintExpr -> Doc
ppConstraintExpr l (CNot e) = mparen l cprioNot $
                              text "not" <+> ppConstraintExpr cprioNot e
ppConstraintExpr l (COp e1 o e2) = mparen l (cprio o) $
                                   ppConstraintExpr (cprio o) e1
                               <+> pp o
                               <+> ppConstraintExpr (cprio o+1) e2
ppConstraintExpr _ (ConstraintExprPrim e) = pp e

cprioNot :: Int
cprioNot = 4

cprio :: COp -> Int
cprio COr = 1
cprio CAnd = 2
-- cprio Not = cprioNot = 4

instance Pp CEqOp where
  pp CEquals   = text "=="
  pp CNotequal = text "!="

instance Pp COp where
  pp CAnd = text "&&"
  pp COr  = text "||"

instance Pp RoleMlsOp where
  pp (CEqOp e) = pp e
  pp Dom       = text "dom"
  pp DomBy     = text "domby"
  pp InComp    = text "incomp"

instance Pp ConstraintExprPrim where
  pp (CUsers o) = ppUserIndex C1 <+> pp o <+> ppUserIndex C2
  pp (CRoles o) = ppRoleIndex C1 <+> pp o <+> ppRoleIndex C2
  pp (CTypes o) = ppTypeIndex C1 <+> pp o <+> ppTypeIndex C2
  pp (CUserSet u o n) = ppUserIndex u <+> pp o <+> pp n
  pp (CRoleSet u o n) = ppRoleIndex u <+> pp o <+> pp n
  pp (CTypeSet u o n) = ppTypeIndex u <+> pp o <+> pp n

ppUserIndex :: ContextIndex -> Doc
ppUserIndex C1 = text "u1"
ppUserIndex C2 = text "u2"
ppUserIndex C3 = text "u3"

ppTypeIndex :: ContextIndex -> Doc
ppTypeIndex C1 = text "t1"
ppTypeIndex C2 = text "t2"
ppTypeIndex C3 = text "t3"

ppRoleIndex :: ContextIndex -> Doc
ppRoleIndex C1 = text "r1"
ppRoleIndex C2 = text "r2"
ppRoleIndex C3 = text "r3"

instance Pp s => Pp (SidContext s) where
  pp (SidContext i sc) = text "sid" <+> pp i <+> pp sc

instance Pp s => Pp (PortContext s) where
  pp (PortContext p from to sc) = text "portcon" <+> pp p <+> pp from <+> maybeto <+> pp sc
   where maybeto | from == to = empty
                 | otherwise  = text "-" <+> pp to

instance Pp s => Pp (NetInterfaceContext s) where
  pp (NetInterfaceContext i intfc packetc) = text "netifcon" <+> pp i <+> pp intfc <+> pp packetc

instance Pp s => Pp (NodeContext s) where
  pp (NodeContext am sc) = text "nodecon" <+> pp am <+> pp sc

instance Pp SecurityContext where
  pp (SecurityContext u r t) = pp u <> colon <> pp r <> colon <> pp t

instance Pp Protocol where
  pp Tcp = text "tcp"
  pp Udp = text "udp"

instance Pp s => Pp (FileSystemUse s) where
  pp (FSUseXattr f sc) = text "fs_use_xattr" <+> pp f <+> pp sc <> semi
  pp (FSUseTask  f sc) = text "fs_use_task"  <+> pp f <+> pp sc <> semi
  pp (FSUseTrans f sc) = text "fs_use_trans" <+> pp f <+> pp sc <> semi

instance Pp s => Pp (GenFileSystemContext s) where
  pp (GenFSCon fs path ftype sc) = text "genfscon" <+> pp fs <+> pp path <+> 
                                   ppMaybeFileType ftype <+> pp sc
   

ppMaybeFileType :: Maybe FileType -> Doc
ppMaybeFileType = maybe empty (\t -> minus <+> pp t)

instance Pp FilePath where
  pp (FilePath s) = text s

instance Pp FileType where
  pp BlockFile     = text "b"
  pp CharacterFile = text "c"
  pp DirectoryFile = text "d"
  pp FifoFile      = text "p"
  pp LinkFile      = text "l"
  pp SocketFile    = text "s"
  pp PlainFile     = text "-"

instance Pp IPV4Address where
  pp (IPV4Address a b c d) = hcat (punctuate period [pp a, pp b, pp c, pp d])

instance Pp IPV6Address where
  pp (IPV6Address a b c d e f g h) = hcat (punctuate colon (map (text . flip showHex "") 
                                                                    [a,b,c,d,e,f,g,h]))

instance Pp IPAddressMask where
  pp (IPV4AddrMask a m) = pp a <+> pp m
  pp (IPV6AddrMask a m) = pp a <+> pp m

instance Pp Word8 where pp p = text (show p)
instance Pp Word16 where pp p = text (show p)

instance Pp Transition where
  pp TypeTransition = text "type_transition"
  pp TypeMember     = text "type_member"
  pp TypeChange     = text "type_change"

instance (Pp st, Pp tt) => Pp (SourceTarget st tt) where
  pp st = pp (sourceTypes st) <+> pp (targetTypes st) <> colon 
       <> pp (targetClasses st)

instance Pp AllowDeny where
  pp Allow      = text "allow"
  pp AuditAllow = text "auditallow"
  pp AuditDeny  = text "auditdeny"
  pp DontAudit  = text "dontaudit"

class Pp i => IsAtom i where
  isAtom :: i -> Bool

instance Pp Self where
  pp (NotSelf t) = pp t
  pp Self        = text "self"

instance IsAtom Self where 
  isAtom (NotSelf t) = isAtom t
  isAtom Self        = True

instance (IsAtom t, Pp t) => Pp (NeverAllow t) where
  pp (NeverAllow t) = pp t
  pp (NAStarTilde t) = pp t

instance IsAtom Identifier where isAtom _ = True

#ifndef __HADDOCK__
deriving instance IsAtom TypeOrAttributeId
deriving instance IsAtom TypeId
deriving instance IsAtom UserId
deriving instance IsAtom ClassId
deriving instance IsAtom PermissionId
deriving instance IsAtom RoleId
#endif

instance IsAtom i => Pp [i] where
  pp [t] | isAtom t = pp t
  pp l = pbracesH (sep (map pp l))

instance IsAtom i => Pp (NonEmptyList i) where
  pp = pp . toList

instance IsAtom i => IsAtom (SignedId i) where
  isAtom (SignedId Positive t) = isAtom t
  isAtom (SignedId Negative _) = False

instance Pp i => Pp (SignedId i) where
  pp (SignedId Positive t) = pp t
  pp (SignedId Negative t)  = minus <> pp t

instance Pp Permissions where
  pp (Permissions l) = pp l
  pp (PStarTilde t) = pp t

instance IsAtom t => Pp (StarTilde t) where
  pp Star = star
  pp (Tilde t) = tilde <> pp t

instance Pp User where
  pp (User u rs) = text "user" <+> pp u <+> text "roles" <+> pp rs <> semi

ppDominance :: Forest RoleId -> Doc
ppDominance [] = semi
ppDominance l  = pbraces [text "role" <+> pp r <+> ppDominance c | Node r c <- l]

instance Pp Identifier where
  pp = text . idString

#ifndef __HADDOCK__
deriving instance Pp ClassId
deriving instance Pp CommonId
deriving instance Pp PermissionId
deriving instance Pp TypeId
deriving instance Pp AttributeId
deriving instance Pp TypeOrAttributeId
deriving instance Pp Sid
deriving instance Pp BoolId
deriving instance Pp UserId
deriving instance Pp RoleId
deriving instance Pp NetInterfaceId
deriving instance Pp FileSystemId
#endif
