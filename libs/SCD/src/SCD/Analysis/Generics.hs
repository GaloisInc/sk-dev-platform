{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module SCD.Analysis.Generics(module SCD.Analysis.Generics, newStdGen) where

-- Experimenting with the Generics extension (-fgenerics) of GHC to
-- define traversals over syntax trees.

import GHC.Base
import GHC.Word(Word8,Word16)
import Prelude hiding (FilePath)
import System.Random(split, next, StdGen, randomR)
import System.Random(newStdGen)
import Test.QuickCheck(Gen,rand,Arbitrary(..), sized)
import Control.Monad(ap,mzero,mplus)
import Data.NonEmptyList(NonEmptyList)

import SCD.SELinux.Syntax
import Data.Tree(Tree)

-- Compute the size of a policy (approximate number of constructors)
class Size a where
  size :: a -> Int
#ifndef __HADDOCK__
  size {| Unit |} _ = 1
  size {| a :*: b |} (x :*: y) = size x + size y + 1
  size {| a :+: b |} (Inl x)  = size x + 1
  size {| a :+: b |} (Inr y) = size y + 1
#endif

data A = A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8
  deriving Show
instance Size A

instance (Size a, Size b) => Size (SourceTarget a b)
instance Size AllowDeny
instance Size AvPerm
instance Size AvRule
instance Size AvRuleBlock
instance Size CEqOp
instance Size COp
instance Size CommonPerm
instance Size CondExpr
instance Size Constraint
instance Size ConstraintExpr
instance Size ConstraintExprPrim
instance Size ContextIndex
instance Size FilePath
instance Size FileSystemUse
instance Size FileType
instance Size GenFileSystemContext
instance Size IPV4Address
instance Size IPV6Address
instance Size IPAddressMask
instance Size NetInterfaceContext
instance Size NodeContext
instance Size Op
instance Size Permissions
instance Size Policy
instance Size PortContext
instance Size Protocol
instance Size Require
instance Size RequireStmt
instance Size RoleMlsOp
instance Size SecurityContext
instance Size Self
instance Size SidContext
instance Size Sign
instance Size Stmt
instance Size TeRbac
instance Size Transition
instance Size User
instance Size a => Size (NeverAllow a)
instance Size a => Size (NonEmptyList a)
instance Size a => Size (SignedId a)
instance Size a => Size (StarTilde a)
instance Size a => Size (Tree a)

instance Size Identifier where size i = size (idString i)
instance Size AttributeId
instance Size BoolId
instance Size ClassId
instance Size CommonId
instance Size FileSystemId
instance Size NetInterfaceId
instance Size PermissionId
instance Size RoleId
instance Size Sid
instance Size TypeId
instance Size TypeOrAttributeId
instance Size UserId

instance Size a => Size (Maybe a)
instance Size Bool
instance Size a => Size [a]
instance Size GHC.Word.Word8 where size _ = 1
instance Size GHC.Word.Word16 where size _ = 1
instance Size Char where size _ = 1

instance Size ()
instance (Size a, Size b) => Size (a,b)
instance (Size a, Size b, Size c) => Size (a,b,c)
instance (Size a, Size b) => Size (Either a b)

-- Collect all identifiers from a policy
class Collect a where
  collect :: a -> [Identifier] -> [Identifier]
#ifndef __HADDOCK__
  collect {| Unit |} _ = id
  collect {| a :*: b |} (x :*: y) = collect x . collect y
  collect {| a :+: b |} (Inl x)  = collect x
  collect {| a :+: b |} (Inr y) = collect y
#endif

instance (Collect a, Collect b) => Collect (SourceTarget a b)
instance Collect AllowDeny
instance Collect AvPerm
instance Collect AvRule
instance Collect AvRuleBlock
instance Collect CEqOp
instance Collect COp
instance Collect CommonPerm
instance Collect CondExpr
instance Collect Constraint
instance Collect ConstraintExpr
instance Collect ConstraintExprPrim
instance Collect ContextIndex
instance Collect FilePath
instance Collect FileSystemUse
instance Collect FileType
instance Collect GenFileSystemContext
instance Collect IPV4Address
instance Collect IPV6Address
instance Collect IPAddressMask
instance Collect NetInterfaceContext
instance Collect NodeContext
instance Collect Op
instance Collect Permissions
instance Collect Policy
instance Collect PortContext
instance Collect Protocol
instance Collect Require
instance Collect RequireStmt
instance Collect RoleMlsOp
instance Collect SecurityContext
instance Collect Self
instance Collect SidContext
instance Collect Sign
instance Collect Stmt
instance Collect TeRbac
instance Collect Transition
instance Collect User
instance Collect a => Collect (NeverAllow a)
instance Collect a => Collect (NonEmptyList a)
instance Collect a => Collect (SignedId a)
instance Collect a => Collect (StarTilde a)
instance Collect a => Collect (Tree a)

instance Collect AttributeId 
  where collect a = (toId a:)
instance Collect BoolId
  where collect a = (toId a:)
instance Collect ClassId
  where collect a = (toId a:)
instance Collect CommonId
  where collect a = (toId a:)
instance Collect FileSystemId
  where collect a = (toId a:)
instance Collect NetInterfaceId
  where collect a = (toId a:)
instance Collect PermissionId
  where collect a = (toId a:)
instance Collect RoleId
  where collect a = (toId a:)
instance Collect Sid
  where collect a = (toId a:)
instance Collect TypeId
  where collect a = (toId a:)
instance Collect TypeOrAttributeId
  where collect a = (toId a:)
instance Collect UserId
  where collect a = (toId a:)

instance Collect a => Collect (Maybe a)
instance Collect Bool
instance Collect a => Collect [a]
instance (Collect a, Collect b) => Collect (Either a b)
instance (Collect a, Collect b) => Collect (a,b)
instance Collect GHC.Word.Word8 where collect _ = id
instance Collect GHC.Word.Word16 where collect _ = id
instance Collect Char where collect _ = id

class Tag a where
    nCons :: a -> Int
#ifndef __HADDOCK__
    nCons {| Unit |}    _ =  1
    nCons {| a :*: b |} _ =  1
    nCons {| a :+: b |} _ = nCons (undefined::a) + nCons (undefined::b)
  
    tag :: a -> Int
    tag {| Unit |}    _       = 0
    tag {| a :*: b |} _       = 0   
    tag {| a :+: b |} (Inl x) = tag x
    tag {| a :+: b |} (Inr y) = nCons (undefined::a) + tag y
#endif

{-
class Cps a where
  cps :: (a -> k) -> k
  cps {| Unit |} = \k -> k Unit
  cps {| a :*: b |} = \k -> cps $ \x ->
                            cps $ \y -> k (x :*: y)
  cps {| a :+: b |} = \k -> cps $ \b -> 
                            if b then cps $ \x -> k (Inl x)
                                 else cps $ \y -> k (Inr y)
-}
{-
class A r where
  g :: (Bool -> r) -> r

instance A (IO a) where
  g k = randomIO >>= k

class C a where
  cps :: A r => (a -> r) -> r
  cps {| Unit |} = \k -> k Unit
  cps {| a :*: b |} = \k -> cps $ \x -> cps $ \y -> k (x :*: y)
  cps {| a :+: b |} = \k -> g $ \b -> if 
                            b then cps $ \x -> k (Inl x)
                              else cps $ \y -> k (Inr y)

instance C Bool
instance C a => C (Maybe a)
instance C a => C [a]
instance (C a, C b) => C (a,b)
instance (C a, C b) => C (Either a b)
-}

-- Generate random data, of arbitrary size
class G a where
  g :: StdGen -> a
#ifndef __HADDOCK__
  g {| Unit |}    _ = Unit
  g {| a :*: b |} r = g r1 :*: g r2 
                      where (r1,r2) = split r
  g {| a :+: b |} r = if odd i then Inl (g r1) else Inr (g r1) 
                      where (i,r1) = next r
#endif

instance G Bool
instance G a => G (Maybe a)
instance G a => G [a]
instance (G a, G b) => G (a,b)
instance (G a, G b) => G (Either a b)

class Gs a where
  gs :: Int -> Int -> StdGen -> ([a],[a])
#ifndef __HADDOCK__
  gs {| Unit |}    l h _ = if l <= 0 && h >= 0 then ([Unit],[]) else ([],[Unit])
  gs {| a :*: b |} l h r = (p ag bg ++ p ag bb ++ p ab bg,p ab bb)
                      where (r1,r2) = split r
                            (ag,ab) :*: (bg,bb) = gs (l-1) (h-1) r1 :*: gs (l-1) (h-1) r2 
                            p l1 l2 = [(x :*: y) | x <- l1, y <- l2]
  gs {| a :+: b |} l h r = if odd i then (tag++tbg,tab++tbb)
                                    else (tbg++tag,tbb++tab)
                      where (ag,ab) = gs (l-1) (h-1) r1
                            (bg,bb) = gs (l-1) (h-1) r1
                            tag = map Inl ag
                            tab = map Inl ab
                            tbg = map Inr bg
                            tbb = map Inr bb
                            (i,r1) = next r
#endif

gsp :: Gs a => Int -> Int -> StdGen -> a
gsp l h r = head (a++b) where (a,b) = gs l h r

instance Gs Bool
instance Gs a => Gs (Maybe a)
instance Gs a => Gs [a]
instance (Gs a, Gs b) => Gs (a,b)
instance (Gs a, Gs b) => Gs (Either a b)

data T a = Nil | Node a (T a) (T a)
  deriving (Eq,Show)

instance Size a => Size (T a)

gt :: Int -> Int -> StdGen -> Maybe (T ())
gt l h r = if h < 0 then mzero else
             if odd i then gtnil `mplus` gtcons
                      else gtcons `mplus` gtnil
  where gtnil = if l < 0 then return Nil else mzero
        gtcons = Just (Node ()) `ap` gt (l-1) (h-1) r1 `ap` gt (l-1) (h-1) r3
        (r1,r2) = split r
        (i,r3) = next r2

instance G a => G (T a)
instance Gs a => Gs (T a)

garbitrary :: G a => Gen a
garbitrary = g `fmap` rand

instance (G a, Arbitrary a) => Arbitrary (T a)
  where arbitrary = garbitrary
        coarbitrary = undefined

data Constrs = UnitC | SplitC (Int,Constrs) (Int,Constrs)
  deriving (Eq,Show)

cutoff :: Int
cutoff = 20

maxL :: Limit a => a -> Int
maxL = maxLn cutoff

c :: Int -> Int
c x = if x >= cutoff then maxBound else x

minL :: Limit a => a -> Int
minL = minLn cutoff

class Limit a where
  maxLn :: Int -> a -> Int
#ifndef __HADDOCK__
  maxLn {| Unit |}    _ _ = 1
  maxLn {| a :*: b |} 1 _ = 1
  maxLn {| a :*: b |} m _ = 1+u+v
                             where u = maxLn (m-1) (undefined::a)
                                   v = maxLn (max 1 (m-1-u)) (undefined::b)
  maxLn {| a :+: b |} 1 _ = 1
  maxLn {| a :+: b |} m _ = 1+max u v
                             where u = maxLn (m-1) (undefined :: a)
                                   v = maxLn (m-1) (undefined :: b)

  minLn :: Int -> a -> Int
  minLn {| Unit |}    _ _ = 1
  minLn {| a :*: b |} 1 _ = 1
  minLn {| a :*: b |} m _ = 1+u+v
                             where u = minLn (m-1) (undefined::a)
                                   v = minLn (max 1 (m-1-u)) (undefined::b)
  minLn {| a :+: b |} 1 _ = 1
  minLn {| a :+: b |} m _ = 1+min u v
                             where u = minLn (m-1) (undefined :: a)
                                   v = minLn (m-1) (undefined :: b)
#endif

instance Limit ()
instance Limit Bool
instance Limit a => Limit (Maybe a)
instance Limit a => Limit [a]
instance (Limit a, Limit b) => Limit (a,b)
instance (Limit a, Limit b) => Limit (Either a b)
instance Limit a => Limit (T a)

class Limit a => G2 a where
  g2 :: Int -> StdGen -> a
#ifndef __HADDOCK__
  g2 {| Unit |}     _ _ = Unit
  g2 {| a :*: b |} m r = g2 ml rl :*: g2 mr rr
                        where l1 = max mina (min maxa (m - maxb - 2))
                              l2 = min maxa (m - minb - 2)
                              (ml,r2) = randomR (l1,l2) r
                              (rl,rr) = split r2
                              mr = max minb (min maxb (m - ml - 1))
                              maxa = c (maxL (undefined::a))
                              mina = minL (undefined::a)
                              maxb = c (maxL (undefined::b))
                              minb = minL (undefined::b)

  g2 {| a :+: b |} m r | (m > maxa || m > maxb) && maxa > maxb  = left
                       | (m > maxa || m > maxb) && maxa < maxb  = right
                       | (m < mina || m < minb) && mina < minb  = left
                       | (m < mina || m < minb) && mina > minb  = right
                       | odd i                    = left
                       | otherwise                = right
                        where left  = Inl (g2 (m-1) rl)
                              right = Inr (g2 (m-1) rr)
                              (i,r2) = next r
                              (rl,rr) = split r2
                              maxa = c (maxL (undefined::a)+1)
                              mina = minL (undefined::a)+1
                              maxb = c (maxL (undefined::b)+1)
                              minb = minL (undefined::b)+1
#endif
{-
m =         1      100    25
u = 3..5    1..1   3..5   3..5   max 1 (min u (m - v - 1)) .. max 1 (min u (m-2))
v = 10..50  1..1   10..50 max 1 (m - u' - 1) (19..23)
-}

instance G2 ()
instance G2 Bool
instance G2 a => G2 (Maybe a)
instance G2 a => G2 [a]
instance (G2 a, G2 b) => G2 (a,b)
instance (G2 a, G2 b) => G2 (Either a b)
instance G2 a => G2 (T a)

g2arbitrary :: G2 a => Gen a
g2arbitrary = 
  do r <- rand
     s <- sized return
     return (g2 s r)               

{-
m =      100    25
u = 5    Inl 50 Inl 4
v = 50          Inr 24
-}

{- The type Gen a is not supported (yet) by -fgenerics
class Tag a => Arb a where
  arb :: Gen a
  arb {| Unit |} a = return a
  arb {| a :*: b |} (x :*: y) = do gx <- arb x; gy <- arb y; return (gx :*: gy)
  arb {| a :+: b |} (Inl x)  = arb x
  arb {| a :+: b |} (Inr y) = arb y

  coarb :: a -> Gen b -> Gen b
  coarb v = variant (tag v) . coarb' v

  coarb' :: a -> Gen b -> Gen b
  coarb' {| Unit |} a = id
  coarb' {| a :*: b |} (x :*: y) = coarb x . coarb y
  coarb' {| a :+: b |} (Inl x) = coarb' x
  coarb' {| a :+: b |} (Inr y) = coarb' y
-}

-- Collect all statements from a policy
class CollectStmt a where
  collectStmt :: a -> [Stmt] -> [Stmt]
#ifndef __HADDOCK__
  collectStmt {| Unit |} _ = id
  collectStmt {| a :*: b |} (x :*: y) = collectStmt x . collectStmt y
  collectStmt {| a :+: b |} (Inl x)  = collectStmt x
  collectStmt {| a :+: b |} (Inr y) = collectStmt y
#endif

instance (CollectStmt a, CollectStmt b) => CollectStmt (SourceTarget a b)
instance CollectStmt AllowDeny
instance CollectStmt AvPerm
instance CollectStmt AvRule
instance CollectStmt AvRuleBlock
instance CollectStmt CEqOp
instance CollectStmt COp
instance CollectStmt CommonPerm
instance CollectStmt CondExpr
instance CollectStmt Constraint
instance CollectStmt ConstraintExpr
instance CollectStmt ConstraintExprPrim
instance CollectStmt ContextIndex
instance CollectStmt FilePath
instance CollectStmt FileSystemUse
instance CollectStmt FileType
instance CollectStmt GenFileSystemContext
instance CollectStmt IPV4Address
instance CollectStmt IPV6Address
instance CollectStmt IPAddressMask
instance CollectStmt NetInterfaceContext
instance CollectStmt NodeContext
instance CollectStmt Op
instance CollectStmt Permissions
instance CollectStmt Policy
instance CollectStmt PortContext
instance CollectStmt Protocol
instance CollectStmt Require
instance CollectStmt RequireStmt
instance CollectStmt RoleMlsOp
instance CollectStmt SecurityContext
instance CollectStmt Self
instance CollectStmt SidContext
instance CollectStmt Sign
instance CollectStmt Stmt where collectStmt s = (s:)
instance CollectStmt TeRbac
instance CollectStmt Transition
instance CollectStmt User
instance CollectStmt a => CollectStmt (NeverAllow a)
instance CollectStmt a => CollectStmt (NonEmptyList a)
instance CollectStmt a => CollectStmt (SignedId a)
instance CollectStmt a => CollectStmt (StarTilde a)
instance CollectStmt a => CollectStmt (Tree a)

instance CollectStmt Identifier where collectStmt _ = id
instance CollectStmt AttributeId 
instance CollectStmt BoolId
instance CollectStmt ClassId
instance CollectStmt CommonId
instance CollectStmt FileSystemId
instance CollectStmt NetInterfaceId
instance CollectStmt PermissionId
instance CollectStmt RoleId
instance CollectStmt Sid
instance CollectStmt TypeId
instance CollectStmt TypeOrAttributeId
instance CollectStmt UserId

instance CollectStmt a => CollectStmt (Maybe a)
instance CollectStmt Bool
instance CollectStmt a => CollectStmt [a]
instance (CollectStmt a, CollectStmt b) => CollectStmt (Either a b)
instance (CollectStmt a, CollectStmt b) => CollectStmt (a,b)
instance CollectStmt GHC.Word.Word8 where collectStmt _ = id
instance CollectStmt GHC.Word.Word16 where collectStmt _ = id
instance CollectStmt Char where collectStmt _ = id

-- Collect all TeRbacs from a policy
class CollectTeRbac a where
  collectTeRbac :: a -> [TeRbac] -> [TeRbac]
#ifndef __HADDOCK__
  collectTeRbac {| Unit |} _ = id
  collectTeRbac {| a :*: b |} (x :*: y) = collectTeRbac x . collectTeRbac y
  collectTeRbac {| a :+: b |} (Inl x)  = collectTeRbac x
  collectTeRbac {| a :+: b |} (Inr y) = collectTeRbac y
#endif

instance (CollectTeRbac a, CollectTeRbac b) => CollectTeRbac (SourceTarget a b)
instance CollectTeRbac AllowDeny
instance CollectTeRbac AvPerm
instance CollectTeRbac AvRule
instance CollectTeRbac AvRuleBlock
instance CollectTeRbac CEqOp
instance CollectTeRbac COp
instance CollectTeRbac CommonPerm
instance CollectTeRbac CondExpr
instance CollectTeRbac Constraint
instance CollectTeRbac ConstraintExpr
instance CollectTeRbac ConstraintExprPrim
instance CollectTeRbac ContextIndex
instance CollectTeRbac FilePath
instance CollectTeRbac FileSystemUse
instance CollectTeRbac FileType
instance CollectTeRbac GenFileSystemContext
instance CollectTeRbac IPV4Address
instance CollectTeRbac IPV6Address
instance CollectTeRbac IPAddressMask
instance CollectTeRbac NetInterfaceContext
instance CollectTeRbac NodeContext
instance CollectTeRbac Op
instance CollectTeRbac Permissions
instance CollectTeRbac Policy
instance CollectTeRbac PortContext
instance CollectTeRbac Protocol
instance CollectTeRbac Require
instance CollectTeRbac RequireStmt
instance CollectTeRbac RoleMlsOp
instance CollectTeRbac SecurityContext
instance CollectTeRbac Self
instance CollectTeRbac SidContext
instance CollectTeRbac Sign
instance CollectTeRbac Stmt
instance CollectTeRbac TeRbac where collectTeRbac s = (s:)
instance CollectTeRbac Transition
instance CollectTeRbac User
instance CollectTeRbac a => CollectTeRbac (NeverAllow a)
instance CollectTeRbac a => CollectTeRbac (NonEmptyList a)
instance CollectTeRbac a => CollectTeRbac (SignedId a)
instance CollectTeRbac a => CollectTeRbac (StarTilde a)
instance CollectTeRbac a => CollectTeRbac (Tree a)

instance CollectTeRbac Identifier where collectTeRbac _ = id
instance CollectTeRbac AttributeId 
instance CollectTeRbac BoolId
instance CollectTeRbac ClassId
instance CollectTeRbac CommonId
instance CollectTeRbac FileSystemId
instance CollectTeRbac NetInterfaceId
instance CollectTeRbac PermissionId
instance CollectTeRbac RoleId
instance CollectTeRbac Sid
instance CollectTeRbac TypeId
instance CollectTeRbac TypeOrAttributeId
instance CollectTeRbac UserId

instance CollectTeRbac a => CollectTeRbac (Maybe a)
instance CollectTeRbac Bool
instance CollectTeRbac a => CollectTeRbac [a]
instance (CollectTeRbac a, CollectTeRbac b) => CollectTeRbac (Either a b)
instance (CollectTeRbac a, CollectTeRbac b) => CollectTeRbac (a,b)
instance CollectTeRbac GHC.Word.Word8 where collectTeRbac _ = id
instance CollectTeRbac GHC.Word.Word16 where collectTeRbac _ = id
instance CollectTeRbac Char where collectTeRbac _ = id
