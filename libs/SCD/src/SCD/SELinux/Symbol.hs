{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{- |
Module      :  $Header$
Description :  SELinux symbol tables
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Creates the SELinux symbol table from the abstract syntax tree.
-}
module SCD.SELinux.Symbol(
  Addable,
  IsSymSet(..),
  PermissionSym,
  SidSym,
  RoleSym,
  UserSym,
  BoolSym,
  Normalizable,
  normalize,
  SymbolTable,
  SymSet,
  SymMap,
  empty,
  build,
  summarize,
  foldTypes,
  classifyTypeOrAttribute,
  allClassPermissions,
  classPermissions) where

import SCD.SELinux.Syntax
import SCD.SELinux.Monad

import Control.Monad(ap,liftM)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Prelude hiding (FilePath, mapM_)
import Data.NonEmptyList(NonEmptyList, FromList, fromList)
import Data.Foldable(mapM_, foldlM, toList, Foldable)
import Data.Traversable(Traversable, traverse)

type SymMap i a = Map.Map i a

emptySymMap :: SymMap i a
emptySymMap = Map.empty

nullSymMap :: SymMap i a -> Bool
nullSymMap m = Map.null m

sizeSymMap :: SymMap i a -> Int
sizeSymMap m = Map.size m

foldSymMap :: (i -> a -> s -> s) -> s -> SymMap i a -> s
foldSymMap = Map.foldWithKey

foldSymMapM :: Monad m => (i -> a -> s -> m s) -> s -> SymMap i a -> m s
foldSymMapM f b m =
    Map.foldWithKey f' (return b) m
    where
      f' i x mz =
          do z <- mz
             f i x z

memberSymMap :: IsIdentifier i => i -> SymMap i a -> Bool
memberSymMap i m = Map.member i m

insertSymMap :: IsIdentifier i => i -> a -> SymMap i a -> P (SymMap i a)
insertSymMap i x m =
    if Map.member i m then
        throwError $ "duplicate identifiers \"" ++ show i ++ "\""
    else
        return (Map.insert i x m)

singletonSymMap :: i -> a -> SymMap i a
singletonSymMap i x = Map.singleton i x

lookupSymMap :: IsIdentifier i => i -> SymMap i a -> Maybe a
lookupSymMap i m = Map.lookup i m

getSymMap :: IsIdentifier i => i -> SymMap i a -> P a
getSymMap i m =
    case lookupSymMap i m of
      Nothing -> throwError $ "no such identifier \"" ++ show i ++ "\""
      Just a -> return a

replaceSymMap :: IsIdentifier i => i -> a -> SymMap i a -> P (SymMap i a)
replaceSymMap i x m =
    if Map.member i m then
        return (Map.insert i x m)
    else
        throwError $ "no such identifier \"" ++ show i ++ "\""

filterSymMap :: IsIdentifier i => (i -> a -> Bool) -> SymMap i a -> SymMap i a
filterSymMap p =
    foldSymMap f emptySymMap
    where
      f i x m = if p i x then Map.insert i x m else m

filterSymMapM :: (Monad m, IsIdentifier i) =>
                 (i -> a -> m Bool) -> SymMap i a -> m (SymMap i a)
filterSymMapM p =
    foldSymMapM f emptySymMap
    where
      f i x m =
          do b <- p i x
             return (if b then Map.insert i x m else m)

unionWithKeySymMap ::
    IsIdentifier i =>
    (i -> a -> a -> a) -> SymMap i a -> SymMap i a -> SymMap i a
unionWithKeySymMap f m1 m2 = Map.unionWithKey f m1 m2

intersectWithKeySymMap ::
    IsIdentifier i =>
    (i -> a -> a -> a) -> SymMap i a -> SymMap i a -> SymMap i a
intersectWithKeySymMap f m1 m2 = Map.intersectionWithKey f m1 m2

type SymSet i = SymMap i ()

class IsIdentifier i => IsSymSet i a | a -> i, i -> a where
    fromSymSet :: SymSet i -> a

    toSymSet :: a -> SymSet i

    emptySymSet :: a
    emptySymSet = fromSymSet emptySymMap

    nullSymSet :: a -> Bool
    nullSymSet s = nullSymMap (toSymSet s)

    sizeSymSet :: a -> Int
    sizeSymSet s = sizeSymMap (toSymSet s)

    insertSymSet :: i -> a -> P a
    insertSymSet i s =
        do m <- insertSymMap i () (toSymSet s)
           return (fromSymSet m)

    insertListSymSet :: Foldable l => l i -> a -> P a
    insertListSymSet i s = foldlM (flip insertSymSet) s i

    singletonSymSet :: i -> a
    singletonSymSet i = fromSymSet (singletonSymMap i ())

    fromListSymSet :: Foldable l => l i -> P a
    fromListSymSet i = insertListSymSet i emptySymSet

    memberSymSet :: i -> a -> Bool
    memberSymSet i s = memberSymMap i (toSymSet s)

    checkSymSet :: i -> a -> P ()
    checkSymSet i s =
        if memberSymSet i s then return ()
        else throwError $ "non-existent identifier \"" ++ show i ++ "\""

    -- A version of insert that permits multiple declarations
    -- returns True if identifier was new
    insertIfNewSymSet :: i -> a -> P (Bool,a)
    insertIfNewSymSet i s =
        if memberSymSet i s then return (False,s)
        else do s' <- insertSymSet i s
                return (True,s')

    -- returns True if any identifier was new
    insertListIfNewSymSet :: Foldable l => l i -> a -> P (Bool,a)
    insertListIfNewSymSet = foldOrM insertIfNewSymSet

    foldSymSet :: (i -> x -> x) -> x -> a -> x
    foldSymSet f b s =
        foldSymMap f' b (toSymSet s)
        where
          f' i () x = f i x

    filterSymSet :: (i -> Bool) -> a -> a
    filterSymSet p s =
        fromSymSet (filterSymMap p' (toSymSet s))
        where
          p' i () = p i

    filterSymSetM :: Monad m => (i -> m Bool) -> a -> m a
    filterSymSetM p s =
        liftM fromSymSet $ filterSymMapM p' (toSymSet s)
        where
          p' i () = p i

    unionSymSet :: a -> a -> a
    unionSymSet s1 s2 =
        fromSymSet
          (unionWithKeySymMap (\_ () () -> ()) (toSymSet s1) (toSymSet s2))

    intersectSymSet :: a -> a -> a
    intersectSymSet s1 s2 =
        fromSymSet
          (intersectWithKeySymMap (\_ () () -> ()) (toSymSet s1) (toSymSet s2))

    disjointSymSet :: a -> a -> Bool
    disjointSymSet s1 s2 = nullSymSet (intersectSymSet s1 s2)

    prettyPrintSymSet :: a -> String
    prettyPrintSymSet =
        \s -> foldSymSet inc "{" s ++ " }"
        where
          inc i s = s ++ " " ++ idString i

newtype PermissionSym = PermissionSym (SymSet PermissionId)
    deriving (Read, Show, Eq)

instance IsSymSet PermissionId PermissionSym where
    fromSymSet s = PermissionSym s
    toSymSet (PermissionSym s) = s

newtype CommonSym = CommonSym (SymMap CommonId PermissionSym)
    deriving (Read, Show)

emptyCommonSym :: CommonSym
emptyCommonSym = CommonSym emptySymMap

sizeCommonSym :: CommonSym -> Int
sizeCommonSym (CommonSym m) = sizeSymMap m

insertCommonSym :: CommonId -> PermissionSym -> CommonSym -> P CommonSym
insertCommonSym c ps (CommonSym m) = liftM CommonSym $ insertSymMap c ps m

getCommonSym :: CommonId -> CommonSym -> P PermissionSym
getCommonSym c (CommonSym m) = getSymMap c m

newtype ClassSym = ClassSym (SymMap ClassId PermissionSym)
    deriving (Read, Show)

emptyClassSym :: ClassSym
emptyClassSym = ClassSym emptySymMap

sizeClassSym :: ClassSym -> Int
sizeClassSym (ClassSym m) = sizeSymMap m

countPermissionsClassSym :: ClassSym -> Int
countPermissionsClassSym (ClassSym m) =
    foldSymMap (\_ s n -> sizeSymSet s + n) 0 m

insertClassSym :: ClassId -> ClassSym -> P ClassSym
insertClassSym c (ClassSym m) =
    liftM ClassSym $ insertSymMap c emptySymSet m

lookupClassSym :: ClassId -> ClassSym -> Maybe PermissionSym
lookupClassSym cl (ClassSym m) = lookupSymMap cl m

{-
memberClassSym :: ClassId -> ClassSym -> Bool
memberClassSym cl cs = Maybe.isJust (lookupClassSym cl cs)
-}

getClassSym :: ClassId -> ClassSym -> P PermissionSym
getClassSym cl cs =
    case lookupClassSym cl cs of
      Just p -> return p
      Nothing -> throwError $ "class does not exist: \"" ++ idString cl ++ "\""

checkClassSym :: ClassId -> ClassSym -> P ()
checkClassSym cl cs =
    do _ <- getClassSym cl cs
       return ()

replaceClassSym :: ClassId -> PermissionSym -> ClassSym -> P ClassSym
replaceClassSym c p (ClassSym m) = liftM ClassSym $ replaceSymMap c p m

initializeClassSym :: ClassId -> PermissionSym -> ClassSym -> P ClassSym
initializeClassSym c p cs =
    do p' <- getClassSym c cs
       if nullSymSet p' then return () else
           throwError $
              "already initialized AV of class \"" ++ idString c ++ "\""
       replaceClassSym c p cs

newtype SidSym = SidSym (SymSet Sid)
    deriving (Read, Show)

instance IsSymSet Sid SidSym where
    fromSymSet s = SidSym s
    toSymSet (SidSym s) = s

newtype RoleSym = RoleSym (SymSet RoleId)
    deriving (Read, Show)

instance IsSymSet RoleId RoleSym where
    fromSymSet s = RoleSym s
    toSymSet (RoleSym s) = s

data TypeAttributeInfo =
    TaiType
  | TaiTypeAlias TypeId
  | TaiAttribute
    deriving (Read, Show)

newtype TypeAttributeSym = TaSym (SymMap Identifier TypeAttributeInfo)
    deriving (Read, Show)

emptyTaSym :: TypeAttributeSym
emptyTaSym = TaSym emptySymMap

sizeTaSym :: TypeAttributeSym -> Int
sizeTaSym (TaSym m) = sizeSymMap m

countTaSym :: (TypeAttributeInfo -> Bool) -> TypeAttributeSym -> Int
countTaSym prd (TaSym m) =
    foldSymMap inc 0 m
    where
      inc _ x n = if prd x then n + 1 else n

countTypesTaSym :: TypeAttributeSym -> Int
countTypesTaSym =
    countTaSym prd
    where
      prd TaiType = True
      prd _ = False

countTypeAliasesTaSym :: TypeAttributeSym -> Int
countTypeAliasesTaSym =
    countTaSym prd
    where
      prd (TaiTypeAlias _) = True
      prd _ = False

countAttributesTaSym :: TypeAttributeSym -> Int
countAttributesTaSym =
    countTaSym prd
    where
      prd TaiAttribute = True
      prd _ = False

foldTypesTaSym :: (TypeId -> a -> a) -> a -> TypeAttributeSym -> a
foldTypesTaSym f =
    \b (TaSym m) -> foldSymMap inc b m
    where
      inc ty TaiType z = f (fromId ty) z
      inc _ _ z = z

insertTypeTaSym :: TypeId -> TypeAttributeSym -> P TypeAttributeSym
insertTypeTaSym ty (TaSym m) =
    liftM TaSym $ insertSymMap (toId ty) TaiType m

insertTypeAliasTaSym :: TypeId -> TypeId -> TypeAttributeSym ->
                        P TypeAttributeSym
insertTypeAliasTaSym ty al (TaSym m) =
    liftM TaSym $ insertSymMap (toId al) (TaiTypeAlias ty) m

insertAttributeTaSym :: AttributeId -> TypeAttributeSym -> P TypeAttributeSym
insertAttributeTaSym att (TaSym m) =
    liftM TaSym $ insertSymMap (toId att) TaiAttribute m

lookupTaSym :: Identifier -> TypeAttributeSym -> Maybe TypeAttributeInfo
lookupTaSym i (TaSym m) = lookupSymMap i m

getTaSym :: Identifier -> TypeAttributeSym -> P TypeAttributeInfo
getTaSym i tas =
    case lookupTaSym i tas of
      Just tai -> return tai
      Nothing -> throwError $ "identifier does not exist: \"" ++ show i ++ "\""

classifyTaSym :: TypeAttributeSym -> TypeOrAttributeId ->
                 P (Either TypeId AttributeId)
classifyTaSym tas ta =
    let i = toId ta in
    do tai <- getTaSym i tas
       case tai of
         TaiAttribute -> return (Right (fromId i))
         _ -> return (Left (fromId i))

lookupTypeTaSym :: TypeId -> TypeAttributeSym -> Maybe (Maybe TypeId)
lookupTypeTaSym ty tas =
    case lookupTaSym (toId ty) tas of
      Just TaiType -> Just Nothing
      Just (TaiTypeAlias ty') -> Just (Just ty')
      _ -> Nothing

isTypeTaSym :: TypeId -> TypeAttributeSym -> Bool
isTypeTaSym ty tas = Maybe.isJust (lookupTypeTaSym ty tas)

normTypeTaSym :: TypeId -> TypeAttributeSym -> P TypeId
normTypeTaSym ty tas =
    case lookupTypeTaSym ty tas of
      Just Nothing -> return ty
      Just (Just ty') -> return ty'
      Nothing -> throwError $ "type does not exist: \"" ++ idString ty ++ "\""

isAttributeTaSym :: AttributeId -> TypeAttributeSym -> Bool
isAttributeTaSym att tas =
    case lookupTaSym (toId att) tas of
      Just TaiAttribute -> True
      _ -> False

checkAttributeTaSym :: AttributeId -> TypeAttributeSym -> P ()
checkAttributeTaSym att tas =
    if isAttributeTaSym att tas then return ()
    else throwError $ "attribute does not exist: \"" ++ idString att ++ "\""

insertTypeIfNewTaSym :: TypeId -> TypeAttributeSym -> P (Bool,TypeAttributeSym)
insertTypeIfNewTaSym ty tas =
    if isTypeTaSym ty tas then return (False,tas)
    else do tas' <- insertTypeTaSym ty tas
            return (True,tas')

insertTypesIfNewTaSym :: Foldable l => l TypeId -> TypeAttributeSym ->
                         P (Bool,TypeAttributeSym)
insertTypesIfNewTaSym = foldOrM insertTypeIfNewTaSym

insertAttributeIfNewTaSym :: AttributeId -> TypeAttributeSym ->
                             P (Bool,TypeAttributeSym)
insertAttributeIfNewTaSym att tas =
    if isAttributeTaSym att tas then return (False,tas)
    else do tas' <- insertAttributeTaSym att tas
            return (True,tas')

insertAttributesIfNewTaSym :: Foldable l => l AttributeId -> TypeAttributeSym ->
                              P (Bool,TypeAttributeSym)
insertAttributesIfNewTaSym = foldOrM insertAttributeIfNewTaSym

newtype UserSym = UserSym (SymSet UserId)
    deriving (Read, Show)

instance IsSymSet UserId UserSym where
    fromSymSet s = UserSym s
    toSymSet (UserSym s) = s


newtype BoolSym = BoolSym (SymSet BoolId)
    deriving (Read, Show)

instance IsSymSet BoolId BoolSym where
    fromSymSet s = BoolSym s
    toSymSet (BoolSym s) = s

data SymbolTable =
    SymbolTable
    {classSym :: ClassSym,
     sidSym :: SidSym,
     commonSym :: CommonSym,
     roleSym :: RoleSym,
     typeAttributeSym :: TypeAttributeSym,
     userSym :: UserSym,
     boolSym :: BoolSym}
    deriving (Read, Show)

empty :: SymbolTable
empty =
    SymbolTable
    {classSym = emptyClassSym,
     sidSym = emptySymSet,
     commonSym = emptyCommonSym,
     roleSym = singletonSymSet (mkId "object_r" :: RoleId),
     typeAttributeSym = emptyTaSym,
     userSym = emptySymSet,
     boolSym = emptySymSet}

foldTypes :: (TypeId -> a -> a) -> a -> SymbolTable -> a
foldTypes f b (SymbolTable {typeAttributeSym = tas}) = foldTypesTaSym f b tas

classifyTypeOrAttribute :: SymbolTable -> TypeOrAttributeId ->
                           P (Either TypeId AttributeId)
classifyTypeOrAttribute (SymbolTable {typeAttributeSym = tas}) ta =
    classifyTaSym tas ta

class Addable a where
    add :: a -> SymbolTable -> P SymbolTable

instance Addable a => Addable [a] where
    add [] sym = return sym
    add (x:xs) sym =
        do sym' <- add x sym
           add xs sym'

instance Addable a => Addable (NonEmptyList a) where
    add = add . toList

instance Addable ClassId where
    add c sym =
        do cs <- insertClassSym c (classSym sym)
           return (sym {classSym = cs})

instance Addable Sid where
    add s sym =
        do ss <- insertSymSet s (sidSym sym)
           return (sym {sidSym = ss})

instance Addable CommonPerm where
    add (CommonPerm c ps) sym =
        do p <- fromListSymSet ps
           cs <- insertCommonSym c p (commonSym sym)
           return (sym {commonSym = cs})

instance Addable AvPerm where
    add (AvPermClass c como) sym =
        do (p,ps) <- case como of
                  Left ps -> return (emptySymSet, toList ps)
                  Right (com, ps) -> do p <- getCommonSym com (commonSym sym)
                                        return (p, ps)
           p' <- insertListSymSet ps p
           cs <- initializeClassSym c p' (classSym sym)
           return (sym {classSym = cs})

addType :: TypeId -> SymbolTable -> P SymbolTable
addType ty sym =
    do tas <- insertTypeTaSym ty (typeAttributeSym sym)
       return (sym {typeAttributeSym = tas})

addTypeAlias :: TypeId -> TypeId -> SymbolTable -> P SymbolTable
addTypeAlias ty al sym =
    do tas <- insertTypeAliasTaSym ty al (typeAttributeSym sym)
       return (sym {typeAttributeSym = tas})

addTypeAliases :: Foldable l =>
                  TypeId -> l TypeId -> SymbolTable -> P SymbolTable
addTypeAliases ty als sym = foldlM (flip (addTypeAlias ty)) sym als

instance Addable AttributeId where
    add att sym =
        do tas <- insertAttributeTaSym att (typeAttributeSym sym)
           return (sym {typeAttributeSym = tas})

instance Addable BoolId where
    add b sym =
        do bs <- insertSymSet b (boolSym sym)
           return (sym {boolSym = bs})

-- Adding roles uses insertIfNew, because roles can be declared multiple times
instance Addable RoleId where
    add r sym =
        do (_,rs) <- insertIfNewSymSet r (roleSym sym)
           return (sym {roleSym = rs})

-- Adding users uses insertIfNew, because users can be declared multiple times
instance Addable UserId where
    add u sym =
        do (_,us') <- insertIfNewSymSet u (userSym sym)
           return (sym {userSym = us'})

instance Addable User where
    add (User u _) sym = add u sym

class Normalizable a where
    normalize :: SymbolTable -> a -> P a

instance Normalizable a => Normalizable [a] where
    normalize sym = traverse (normalize sym)

instance Normalizable a => Normalizable (NonEmptyList a) where
    normalize sym = traverse (normalize sym)

instance Normalizable a => Normalizable (Maybe a) where
    normalize sym = traverse (normalize sym)

instance Normalizable a => Normalizable (Tree.Tree a) where
    normalize sym (Tree.Node {Tree.rootLabel = r, Tree.subForest = l}) =
        do r' <- normalize sym r
           l' <- normalize sym l
           return (Tree.Node {Tree.rootLabel = r', Tree.subForest = l'})

instance Normalizable ClassId where
    normalize (SymbolTable {classSym = cs}) cl =
        do checkClassSym cl cs
           return cl

checkPermission :: SymbolTable -> ClassId -> PermissionId -> P ()
checkPermission (SymbolTable {classSym = cs}) cl p =
    do ps <- getClassSym cl cs
       checkSymSet p ps

checkPermissionList :: Foldable l => SymbolTable -> ClassId -> l PermissionId -> P ()
checkPermissionList sym cl ps = mapM_ (checkPermission sym cl) ps

checkPermissions :: SymbolTable -> ClassId -> Permissions -> P ()
checkPermissions sym cl p =
    case p of
      Permissions ps -> checkPermissionList sym cl ps
      PStarTilde Star -> return ()
      PStarTilde (Tilde ps) -> checkPermissionList sym cl ps

instance Normalizable Sid where
    normalize (SymbolTable {sidSym = ss}) s =
        do checkSymSet s ss
           return s

instance Normalizable TypeId where
    normalize (SymbolTable {typeAttributeSym = tas}) ty =
        normTypeTaSym ty tas

instance Normalizable AttributeId where
    normalize (SymbolTable {typeAttributeSym = tas}) att =
        do checkAttributeTaSym att tas
           return att

instance Normalizable TypeOrAttributeId where
    normalize (SymbolTable {typeAttributeSym = tas}) ta =
        do tai <- getTaSym (toId ta) tas
           case tai of
             TaiType -> return ta
             TaiTypeAlias ty -> return (TypeOrAttributeId (toId ty))
             TaiAttribute -> return ta

instance Normalizable BoolId where
    normalize (SymbolTable {boolSym = bs}) b =
        do checkSymSet b bs
           return b

instance Normalizable RoleId where
    normalize (SymbolTable {roleSym = rs}) r =
        do checkSymSet r rs
           return r

instance Normalizable UserId where
    normalize (SymbolTable {userSym = us}) u =
        do checkSymSet u us
           return u

instance Normalizable FileSystemId where
    normalize _ (i @ (FileSystemId _)) = return i

instance Normalizable NetInterfaceId where
    normalize _ (i @ (NetInterfaceId _)) = return i

instance Normalizable CondExpr where
    normalize sym (Not c) = liftM Not $ normalize sym c
    normalize sym (Op c1 o c2) =
        return Op `ap` normalize sym c1 `ap` normalize sym o `ap`
        normalize sym c2
    normalize sym (Var b) = liftM Var $ normalize sym b

instance Normalizable Op where
    normalize _ And = return And
    normalize _ Or = return Or
    normalize _ Xor = return Xor
    normalize _ Equals = return Equals
    normalize _ Notequal = return Notequal

instance Normalizable Transition where
    normalize _ TypeTransition = return TypeTransition
    normalize _ TypeMember = return TypeMember
    normalize _ TypeChange = return TypeChange

instance Normalizable AllowDeny where
    normalize _ Allow = return Allow
    normalize _ AuditAllow = return AuditAllow
    normalize _ AuditDeny = return AuditDeny
    normalize _ DontAudit = return DontAudit

instance Normalizable Stmt where
    normalize sym (Transition t st ty) =
        return Transition `ap` normalize sym t `ap` normalize sym st `ap`
        normalize sym ty
    normalize sym (TeAvTab ad st perm) =
        do ad' <- normalize sym ad
           st' @ SourceTarget {targetClasses = cls} <- normalize sym st
           mapM_ (flip (checkPermissions sym) perm) cls
           return (TeAvTab ad' st' perm)

instance Normalizable Self where
    normalize sym (NotSelf ta) = liftM NotSelf $ normalize sym ta
    normalize _ Self = return Self

instance Normalizable t => Normalizable (SignedId t) where
    normalize sym (SignedId b x) = liftM (SignedId b) (normalize sym x)

instance Normalizable i => Normalizable (StarTilde i) where
    normalize _ Star = return Star
    normalize sym (Tilde x) = liftM Tilde $ normalize sym x

instance Normalizable t => Normalizable (NeverAllow t) where
    normalize sym (NeverAllow x) = liftM NeverAllow $ normalize sym x
    normalize sym (NAStarTilde x) = liftM NAStarTilde $ normalize sym x

instance (Normalizable st, Normalizable tt) =>
         Normalizable (SourceTarget st tt) where
    normalize sym (SourceTarget {sourceTypes = st,
                                 targetTypes = tt,
                                 targetClasses = cls}) =
        do st' <- normalize sym st
           tt' <- normalize sym tt
           cls' <- normalize sym cls
           return (SourceTarget {sourceTypes = st',
                                 targetTypes = tt',
                                 targetClasses = cls'})

instance Normalizable Constraint where
    normalize sym (Constrain cls ps ce) =
        do cls' <- normalize sym cls
           mapM_ (flip (checkPermissionList sym) ps) cls'
           ce' <- normalize sym ce
           return (Constrain cls' ps ce')
    normalize sym (ValidateTrans cls ce) =
        return ValidateTrans `ap` normalize sym cls `ap` normalize sym ce

instance Normalizable ConstraintExpr where
    normalize sym (ConstraintExprPrim cep) =
        liftM ConstraintExprPrim $ normalize sym cep
    normalize sym (CNot ce) = liftM CNot $ normalize sym ce
    normalize sym (COp ce1 o ce2) =
        return COp `ap` normalize sym ce1 `ap`
        normalize sym o `ap` normalize sym ce2

instance Normalizable ConstraintExprPrim where
    normalize sym (CUsers eo) = liftM CUsers $ normalize sym eo
    normalize sym (CRoles r) = liftM CRoles $ normalize sym r
    normalize sym (CTypes eo) = liftM CTypes $ normalize sym eo
    normalize sym (CUserSet ci eo ul) =
        return CUserSet `ap` normalize sym ci `ap` normalize sym eo `ap`
        normalize sym ul
    normalize sym (CRoleSet ci eo rl) =
        return CRoleSet `ap` normalize sym ci `ap` normalize sym eo `ap`
        normalize sym rl
    normalize sym (CTypeSet ci eo tal) =
        return CTypeSet `ap` normalize sym ci `ap` normalize sym eo `ap`
        normalize sym tal

instance Normalizable ContextIndex where
    normalize _ C1 = return C1
    normalize _ C2 = return C2
    normalize _ C3 = return C3

instance Normalizable COp where
    normalize _ CAnd = return CAnd
    normalize _ COr = return COr

instance Normalizable CEqOp where
    normalize _ CEquals = return CEquals
    normalize _ CNotequal = return CNotequal

instance Normalizable RoleMlsOp where
    normalize sym (CEqOp eo) = liftM CEqOp $ normalize sym eo
    normalize _ Dom = return Dom
    normalize _ DomBy = return DomBy
    normalize _ InComp = return InComp

instance Normalizable s => Normalizable (SidContext s) where
    normalize sym (SidContext s sc) =
        return SidContext `ap` normalize sym s `ap` normalize sym sc

instance Normalizable s => Normalizable (NodeContext s) where
    normalize sym (NodeContext am sc) =
        return NodeContext `ap` normalize sym am `ap` normalize sym sc

instance Normalizable SecurityContext where
    normalize sym (SecurityContext u r t) =
        return SecurityContext `ap` normalize sym u `ap`
        normalize sym r `ap` normalize sym t

instance Normalizable Protocol where
    normalize _ Tcp = return Tcp
    normalize _ Udp = return Udp

instance Normalizable s => Normalizable (FileSystemUse s) where
    normalize sym (FSUseXattr fs sc) =
        return FSUseXattr `ap` normalize sym fs `ap` normalize sym sc
    normalize sym (FSUseTask fs sc) =
        return FSUseTask `ap` normalize sym fs `ap` normalize sym sc
    normalize sym (FSUseTrans fs sc) =
        return FSUseTrans `ap` normalize sym fs `ap` normalize sym sc

instance Normalizable s => Normalizable (GenFileSystemContext s) where
    normalize sym (GenFSCon fs fp ft sc) =
        return GenFSCon `ap` normalize sym fs `ap` normalize sym fp `ap`
        normalize sym ft `ap` normalize sym sc

instance Normalizable FilePath where
    normalize _ (fp @ (FilePath _)) = return fp

instance Normalizable FileType where
    normalize _ BlockFile = return BlockFile
    normalize _ CharacterFile = return CharacterFile
    normalize _ DirectoryFile = return DirectoryFile
    normalize _ FifoFile = return FifoFile
    normalize _ LinkFile = return LinkFile
    normalize _ SocketFile = return SocketFile
    normalize _ PlainFile = return PlainFile

instance Normalizable IPAddressMask where
    normalize sym (IPV4AddrMask a m) = return IPV4AddrMask `ap`
                                       normalize sym a `ap`
                                       normalize sym m
    normalize sym (IPV6AddrMask a m) = return IPV6AddrMask `ap`
                                       normalize sym a `ap`
                                       normalize sym m

instance Normalizable IPV4Address where
    normalize _ (ip @ (IPV4Address _ _ _ _)) = return ip

instance Normalizable IPV6Address where
    normalize _ (ip @ (IPV6Address _ _ _ _ _ _ _ _)) = return ip

instance Normalizable s => Normalizable (PortContext s) where
    normalize sym (PortContext p w1 w2 sc) =
        return PortContext `ap` normalize sym p `ap` return w1 `ap`
        return w2 `ap` normalize sym sc

instance Normalizable s => Normalizable (NetInterfaceContext s) where
    normalize sym (NetInterfaceContext ni sc1 sc2) =
        return NetInterfaceContext `ap` normalize sym ni `ap`
        normalize sym sc1 `ap` normalize sym sc2

-- Returns True if anything had to be added to the symbol table to
-- satisfy the requirement.
satisfyRequire :: Require -> SymbolTable -> P (Bool,SymbolTable)
satisfyRequire req sym =
    case req of
      RequireClass c pl ->
          let SymbolTable {classSym = cs} = sym in
          let (new,ps) =
                case lookupClassSym c cs of
                  Just x -> (False,x)
                  Nothing -> (True, emptySymSet) in
          do cs' <- if new then insertClassSym c cs else return cs
             (new',ps') <- insertListIfNewSymSet pl ps
             cs'' <- replaceClassSym c ps' cs'
             return (new || new', sym {classSym = cs''})
      RequireRole rl ->
          do (new,rs) <- insertListIfNewSymSet rl (roleSym sym)
             return (new, sym {roleSym = rs})
      RequireType tyl ->
          do (new,tas) <- insertTypesIfNewTaSym tyl (typeAttributeSym sym)
             return (new, sym {typeAttributeSym = tas})
      RequireAttribute attl ->
          do (new,tas) <- insertAttributesIfNewTaSym attl (typeAttributeSym sym)
             return (new, sym {typeAttributeSym = tas})
      RequireUser ul ->
          do (new,us) <- insertListIfNewSymSet ul (userSym sym)
             return (new, sym {userSym = us})
      RequireBool bl ->
          do (new,bs) <- insertListIfNewSymSet bl (boolSym sym)
             return (new, sym {boolSym = bs})

foldOrM :: (Foldable l, Monad m) =>
           (a -> s -> m (Bool,s)) -> l a -> s -> m (Bool,s)
foldOrM f = g . toList where
  g [] s = return (False,s)
  g (a:as) s = do (n,s') <- f a s
                  (n',s'') <- g as s'
                  return (n || n', s'')

satisfyRequires :: Foldable l =>
                   l Require -> SymbolTable -> P (Bool,SymbolTable)
satisfyRequires = foldOrM satisfyRequire

normalizeRequireStmts :: SymbolTable -> [RequireStmt] -> P [RequireStmt]
normalizeRequireStmts initSym reqs =
    do (new,rs) <- check initSym reqs
       return (if new then [] else map RequireStmt rs)
    where
      check _ [] = return (False,[])
      check sym (Require r : rs) =
          do (new,sym') <- satisfyRequires r sym
             (new',rs') <- check sym' rs
             return (new || new', rs')
      check sym (RequireStmt r : rs) =
          do r' <- normalize sym r
             (new,rs') <- check sym rs
             return (new, r':rs')

addForwardTeRbac :: TeRbac -> SymbolTable -> P SymbolTable
addForwardTeRbac tr sym =
    case tr of
      Type ty als _ ->
          do sym' <- addType ty sym
             addTypeAliases ty als sym'
      TypeAlias ty als -> addTypeAliases ty als sym
      _ -> return sym

addForwardTeRbacs :: [TeRbac] -> SymbolTable -> P SymbolTable
addForwardTeRbacs trs sym = foldlM (flip addForwardTeRbac) sym trs

addDominance :: (Foldable l, FromList l) =>
                l (Tree.Tree RoleId) ->
                SymbolTable -> SymbolTable -> SymbolTable ->
                P (l (Tree.Tree RoleId), SymbolTable, SymbolTable, SymbolTable)
addDominance l e s d =
  do (l',e',s',d') <- addDominance' (toList l) e s d
     return (fromList l', e', s', d')
  where
  addDominance' [] env sym decls = return ([],env,sym,decls)
  addDominance' (t:ts) env sym decls =
      let Tree.Node {Tree.rootLabel = r, Tree.subForest = rs} = t in
      do env' <- add r env
         sym' <- add r sym
         decls' <- add r decls
         rs' <- normalize env rs  -- r *shouldn't* appear in rs...
         (ts',env'',sym'',decls'') <- addDominance' ts env' sym' decls'
         return (Tree.Node {Tree.rootLabel = r, Tree.subForest = rs'} : ts',
                 env'', sym'', decls'')

-- env: all the symbols in the current scope (normalize wrt this)
-- sym: constructing the final scope with optional blocks
-- decls: all declarations (to check for duplicate declarations)
addTeRbac :: TeRbac -> SymbolTable -> SymbolTable -> SymbolTable ->
             P ([TeRbac],[User],SymbolTable,SymbolTable,SymbolTable)
addTeRbac tr env sym decls =
    case tr of
      Attribute att ->
          do env' <- add att env
             sym' <- add att sym
             decls' <- add att decls
             return ([tr],[],env',sym',decls')
      Type ty als atts ->
          do _ <- normalize env ty
             _ <- normalize env als
             atts' <- normalize env atts
             return ([Type ty als atts'],[],env,sym,decls)
      TypeAlias ty als ->
          do ty' <- normalize env ty
             _ <- normalize env als
             return ([TypeAlias ty' als], [], env, sym, decls)
      TypeAttribute ty atts ->
          do ty' <- normalize env ty
             atts' <- normalize env atts
             return ([TypeAttribute ty' atts'], [], env, sym, decls)
      BoolDef b _ ->
          do env' <- add b env
             sym' <- add b sym
             decls' <- add b decls
             return ([tr],[],env',sym',decls')
      TeNeverAllow srcTarg perm ->
          do srcTarg' @ SourceTarget {targetClasses = cls'} <-
                 normalize sym srcTarg
             mapM_ (flip (checkPermissions env) perm) cls'
             return ([TeNeverAllow srcTarg' perm], [], env, sym, decls)
      Role r tas ->
          do env' <- add r env
             sym' <- add r sym
             decls' <- add r decls
             tas' <- normalize env tas
             return ([Role r tas'], [], env', sym', decls')
      Dominance d ->
          do (d',env',sym',decls') <- addDominance d env sym decls
             return ([Dominance d'], [], env', sym', decls')
      RoleTransition rs tas r ->
          do rs' <- normalize env rs
             tas' <- normalize env tas
             r' <- normalize env r
             return ([RoleTransition rs' tas' r'], [], env, sym, decls)
      RoleAllow rs rt ->
          do rs' <- normalize env rs
             rt' <- normalize env rt
             return ([RoleAllow rs' rt'], [], env, sym, decls)
      CondStmt c t e ->
          do c' <- normalize env c
             t' <- normalizeRequireStmts env t
             e' <- normalizeRequireStmts env e
             return (mkCondStmt c' t' e', [], env, sym, decls)
          where
            mkCondStmt _ [] [] = []
            mkCondStmt (Not c') [] e' = [CondStmt c' e' []]
            mkCondStmt c' [] e' = [CondStmt (Not c') e' []]
            mkCondStmt c' t' e' = [CondStmt c' t' e']
      Stmt s ->
          do s' <- normalize env s
             return ([Stmt s'], [], env, sym, decls)
      Optional t e ->
          do (t_new,t_trs,t_us,t_sym,decls') <- addAvRuleBlock t env sym decls
             (e_new,e_trs,e_us,e_sym,decls'') <-
                 case e of
                   Just x -> addAvRuleBlock x env sym decls'
                   Nothing -> return (False,[],[],sym,decls')
             if not e_new then return () else
                 throwError "optional else block should not contain requires"
             return (if not t_new then (t_trs,t_us,env,t_sym,decls'')
                     else (e_trs,e_us,env,e_sym,decls''))

addTeRbacs :: [TeRbac] -> SymbolTable -> SymbolTable -> SymbolTable ->
                P ([TeRbac],[User],SymbolTable)
addTeRbacs [] _ sym _ = return ([],[],sym)
addTeRbacs (tr:trs) env sym decls =
    do (tr',us,env',sym',decls') <- addTeRbac tr env sym decls
       (trs',us',sym'') <- addTeRbacs trs env' sym' decls'
       return (tr' ++ trs', us ++ us', sym'')

addForwardAvRule :: AvRule -> SymbolTable -> P SymbolTable
addForwardAvRule av sym =
    case av of
      TeRbac tr -> addForwardTeRbac tr sym
      AvRuleRequire _ -> return sym

addForwardAvRules :: [AvRule] -> SymbolTable -> P SymbolTable
addForwardAvRules avs sym = foldlM (flip addForwardAvRule) sym avs

addAvRule :: AvRule ->
             SymbolTable -> SymbolTable -> SymbolTable -> SymbolTable ->
             P (Bool,[TeRbac],[User],SymbolTable,SymbolTable,SymbolTable)
addAvRule av outer env sym decls =
    case av of
      TeRbac tr ->
          do (trs,us,env',sym',decls') <- addTeRbac tr env sym decls
             return (False,trs,us,env',sym',decls')
      AvRuleRequire reqs ->
          do (new,_) <- satisfyRequires reqs outer
             (_,env') <- satisfyRequires reqs env
             return (new,[],[],env',sym,decls)

addAvRules :: [AvRule] ->
              SymbolTable -> SymbolTable -> SymbolTable -> SymbolTable ->
              P (Bool,[TeRbac],[User],SymbolTable,SymbolTable)
addAvRules [] _ _ sym decls = return (False,[],[],sym,decls)
addAvRules (av:avl) outer env sym decls =
    do (new,trs,us,env',sym',decls') <- addAvRule av outer env sym decls
       (new',trs',us',sym'',decls'') <- addAvRules avl outer env' sym' decls'
       return (new || new', trs ++ trs', us ++ us', sym'', decls'')

addAvRuleBlock :: AvRuleBlock -> SymbolTable -> SymbolTable -> SymbolTable ->
                P (Bool,[TeRbac],[User],SymbolTable,SymbolTable)
addAvRuleBlock (AvRuleBlock avs us) env sym decls =
    do env' <- addForwards env
       sym' <- addForwards sym
       decls' <- addForwards decls
       (new,trs,us',sym'',decls'') <- addAvRules avs env env' sym' decls'
       return (new, trs, us' ++ us, sym'', decls'')
    where
      addForwards s =
          do s' <- addForwardAvRules avs s
             add us s'

addPolicy :: Policy -> SymbolTable -> P (Policy,SymbolTable)
addPolicy policy sym =
    let Policy {classes = cs,
                initialSids = iss,
                commonPerms = cps,
                avPerms = avs,
                teRbacs = trs,
                users = us,
                constraints = cons,
                sidContexts = ss,
                fileSystemUses = fsus,
                genFileSystemContexts = gfscs,
                portContexts = pcs,
                netInterfaceContexts = nics,
                nodeContexts = ncs} = policy in
    do sym' <- add cs sym >>=
               add iss >>=
               add cps >>=
               add avs >>=

       -- Users need to be added before TeRbacs (out of order), because
       -- UserIds might appear in TeRbac require blocks
               add us >>=

       -- First TeRbac pass: add symbols that support forward references
               addForwardTeRbacs trs

       -- Second TeRbac pass: ensure all references respect scope and there
       -- are no clashing (duplicate) declarations
       -- Also eliminate type aliases and expand optional blocks
       (trs',us',sym'') <- addTeRbacs trs sym' sym' sym'

       cons' <- normalize sym'' cons
       ss' <- normalize sym'' ss
       fsus' <- normalize sym'' fsus
       gfscs' <- normalize sym'' gfscs
       pcs' <- normalize sym'' pcs
       nics' <- normalize sym'' nics
       ncs' <- normalize sym'' ncs

       return (policy {teRbacs = trs',
                       users = fromList (toList us ++ us'),
                       constraints = cons',
                       sidContexts = ss',
                       fileSystemUses = fsus',
                       genFileSystemContexts = gfscs',
                       portContexts = pcs',
                       netInterfaceContexts = nics',
                       nodeContexts = ncs'}, sym'')

build :: Policy -> Either String (Policy,SymbolTable)
build = runP . flip addPolicy empty

summarize :: SymbolTable -> String
summarize (SymbolTable {classSym = cs,
                        sidSym = ss,
                        commonSym = coms,
                        roleSym = rs,
                        typeAttributeSym = tas,
                        userSym = us,
                        boolSym = bs}) =
    "classes: " ++ show (sizeClassSym cs) ++ "\n" ++
    "--permissions: " ++ show (countPermissionsClassSym cs) ++ "\n" ++
    "sids: " ++ show (sizeSymSet ss) ++ "\n" ++
    "commons: " ++ show (sizeCommonSym coms) ++ "\n" ++
    "roles: " ++ show (sizeSymSet rs) ++ "\n" ++
    "types/attributes: " ++ show (sizeTaSym tas) ++ "\n" ++
    "--types: " ++ show (countTypesTaSym tas) ++ "\n" ++
    "--aliases: " ++ show (countTypeAliasesTaSym tas) ++ "\n" ++
    "--attributes: " ++ show (countAttributesTaSym tas) ++ "\n" ++
    "users: " ++ show (sizeSymSet us) ++ "\n" ++
    "bools: " ++ show (sizeSymSet bs) ++ "\n"

allClassPermissions :: SymbolTable -> ClassId -> P PermissionSym
allClassPermissions symbol cl = getClassSym cl (classSym symbol)

classPermissions :: SymbolTable -> ClassId -> Permissions -> P PermissionSym
classPermissions symbol cl perm =
    do allPerms <- allClassPermissions symbol cl
       return (filterSymSet permFilter allPerms)
    where
      permFilter =
          case perm of
            Permissions ps -> flip elem (toList ps)
            PStarTilde Star -> const True
            PStarTilde (Tilde ps) -> not . flip elem (toList ps)
