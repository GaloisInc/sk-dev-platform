{-# LANGUAGE FlexibleInstances #-}
{- |
Module      :  $Header$
Description :  SELinux authorization relation
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Creates the SELinux authorization relation from the
(simplified) abstract syntax tree and symbol table.
-}
module SCD.SELinux.Authorize(
  Authorize,
  TypeSetifiable,
  Result,
  empty,
  build,
  summarize,
  setBool,
  isAuthorized,
  accessVector) where

import SCD.SELinux.Syntax
import SCD.SELinux.Monad
import qualified SCD.SELinux.Symbol as Symbol

import Control.Monad(ap,foldM,liftM,when)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (FilePath, foldr)
import Data.Strict(returnHyperStrict)
import Data.Foldable(Foldable, toList, foldr, foldlM)
import Data.NonEmptyList(NonEmptyList,fromList)

{-
import Debug.Trace(trace)
import SCD.SELinux.PrettyPrint(prettyPrint)
traceFunction :: Eq a => String -> a -> a
traceFunction n x =
    trace ("entering function " ++ n ++ "\n")
          (x == x `seq` trace ("leaving function " ++ n ++ "\n") x)
-}

anyWithKeyMap :: (k -> a -> Bool) -> Map.Map k a -> Bool
anyWithKeyMap p = Map.foldWithKey (\k v b -> p k v || b) False

allWithKeyMap :: (k -> a -> Bool) -> Map.Map k a -> Bool
allWithKeyMap p m = not $ anyWithKeyMap (\k v -> not $ p k v) m

signedIdPolarity :: SignedId a -> Sign
signedIdPolarity (SignedId b _) = b

signedIdElement :: SignedId a -> a
signedIdElement (SignedId _ x) = x

hasSignedIdPolarity :: Sign -> SignedId a -> Bool
hasSignedIdPolarity b s = signedIdPolarity s == b

destSignedIdPolarity :: Sign -> SignedId a -> Maybe a
destSignedIdPolarity b s =
    if hasSignedIdPolarity b s then Just (signedIdElement s) else Nothing

partitionSignedIds :: [SignedId a] -> ([a],[a])
partitionSignedIds sl =
    (Maybe.mapMaybe (destSignedIdPolarity Positive) sl,
     Maybe.mapMaybe (destSignedIdPolarity Negative) sl)

{-
isSelf :: Self -> Bool
isSelf Self = True
isSelf _ = False
-}

destNotSelf :: Self -> Maybe TypeOrAttributeId
destNotSelf Self = Nothing
destNotSelf (NotSelf a) = Just a

destSignedNotSelf :: SignedId Self -> Maybe (SignedId TypeOrAttributeId)
destSignedNotSelf (SignedId s x) = liftM (SignedId s) $ destNotSelf x

destListSignedSelf :: (Foldable l) => 
                  l (SignedId Self) -> P (Bool, [SignedId TypeOrAttributeId])
destListSignedSelf tt' =
    let tt = toList tt'
        ts = Maybe.mapMaybe destSignedNotSelf tt in
    if length ts == length tt then
        return (False,ts)
    else if any (hasSignedIdPolarity Negative) ts then
        throwError "UNSUPPORTED: self cannot occur with negative types"
    else if any (hasSignedIdPolarity Negative) tt then
        throwError "UNSUPPORTED: self cannot occur negatively"
    else
        return (True,ts)

destStarTildeSignedSelf ::
    StarTilde (SignedId Self) ->
    P (Bool, Maybe (StarTilde (SignedId TypeOrAttributeId)))
destStarTildeSignedSelf st =
    case st of
      Star -> return (False,Just Star)
      Tilde xs ->
          do (self,xs') <- destListSignedSelf xs
             when self $ 
                 throwError "UNSUPPORTED: self cannot occur inside a tilde operator"
             return (self, if null xs' then Nothing 
                           else Just (Tilde (fromList xs')))

destNeverAllowSelf :: NeverAllow Self -> 
                      P (Bool, Maybe (NeverAllow TypeOrAttributeId))
destNeverAllowSelf na =
    case na of
      NeverAllow xs ->
          do (self,xs') <- destListSignedSelf xs
             return (self, if null xs' then Nothing 
                           else Just (NeverAllow (fromList xs')))
      NAStarTilde st ->
          do (self,st') <- destStarTildeSignedSelf st
             return (self, NAStarTilde `fmap` st')

isAllowAllowDeny :: AllowDeny -> P Bool
isAllowAllowDeny ad =
    case ad of
      Allow -> return True
      AuditAllow -> return True
      AuditDeny -> throwError "UNSUPPORTED: auditdeny rule"
      DontAudit -> return False

newtype TypeSet = TypeSet (Set.Set TypeId)
    deriving (Eq, Read, Show)

emptyTypeSet :: TypeSet
emptyTypeSet = TypeSet Set.empty

sizeTypeSet :: TypeSet -> Int
sizeTypeSet (TypeSet s) = Set.size s

memberTypeSet :: TypeId -> TypeSet -> Bool
memberTypeSet t (TypeSet s) = Set.member t s

singletonTypeSet :: TypeId -> TypeSet
singletonTypeSet t = TypeSet (Set.singleton t)

addTypeSet :: TypeId -> TypeSet -> TypeSet
addTypeSet t (TypeSet s) = TypeSet (Set.insert t s)

unionTypeSet :: TypeSet -> TypeSet -> TypeSet
unionTypeSet (TypeSet s1) (TypeSet s2) = TypeSet (Set.union s1 s2)

isSubsetOfTypeSet :: TypeSet -> TypeSet -> Bool
isSubsetOfTypeSet (TypeSet s1) (TypeSet s2) = Set.isSubsetOf s1 s2

differenceTypeSet :: TypeSet -> TypeSet -> P TypeSet
differenceTypeSet (ts1 @ (TypeSet s1)) (ts2 @ (TypeSet s2)) =
    if isSubsetOfTypeSet ts2 ts1 then return (TypeSet (Set.difference s1 s2))
    else throwError $ "UNSUPPORTED: type set difference must be a subset: " ++ show (s1,s2)

foldTypeSet :: (TypeId -> a -> a) -> a -> TypeSet -> a
foldTypeSet f b (TypeSet s) = Set.fold f b s

foldSelfTypeSet ::
    (TypeId -> TypeId -> a -> a) -> a -> TypeSet -> Bool -> TypeSet -> a
foldSelfTypeSet f b stys self ttys =
    foldTypeSet flds b stys
    where
      flds sty z = foldTypeSet (f sty) (if self then f sty sty z else z) ttys

anySelfTypeSet ::
    (TypeId -> TypeId -> Bool) -> TypeSet -> Bool -> TypeSet -> Bool
anySelfTypeSet p stys self ttys =
    foldSelfTypeSet orP False stys self ttys
    where
      orP sty tty b = p sty tty || b

allSelfTypeSet ::
    (TypeId -> TypeId -> Bool) -> TypeSet -> Bool -> TypeSet -> Bool
allSelfTypeSet p stys self ttys =
    not $ anySelfTypeSet notP stys self ttys
    where
      notP sty tty = not $ p sty tty

allTypes :: Symbol.SymbolTable -> TypeSet
allTypes = Symbol.foldTypes addTypeSet emptyTypeSet

newtype RoleSet = RoleSet (Set.Set RoleId)
    deriving (Eq, Read, Show)

emptyRoleSet :: RoleSet
emptyRoleSet = RoleSet Set.empty

sizeRoleSet :: RoleSet -> Int
sizeRoleSet (RoleSet s) = Set.size s

memberRoleSet :: RoleId -> RoleSet -> Bool
memberRoleSet r (RoleSet s) = Set.member r s

addRoleSet :: RoleId -> RoleSet -> RoleSet
addRoleSet r (RoleSet s) = RoleSet (Set.insert r s)

unionRoleSet :: RoleSet -> RoleSet -> RoleSet
unionRoleSet (RoleSet s1) (RoleSet s2) = RoleSet (Set.union s1 s2)

newtype AttributeSet = AttributeSet (Map.Map AttributeId TypeSet)
    deriving (Eq, Read, Show)

emptyAttributeSet :: AttributeSet
emptyAttributeSet = AttributeSet Map.empty

sizeAttributeSet :: AttributeSet -> Int
sizeAttributeSet (AttributeSet m) = Map.size m

countUniqueTypesAttributeSet :: AttributeSet -> Int
countUniqueTypesAttributeSet (AttributeSet m) =
    sizeTypeSet $ Map.fold unionTypeSet emptyTypeSet m

getAttributeSet :: AttributeSet -> AttributeId -> TypeSet
getAttributeSet (AttributeSet m) a =
    case Map.lookup a m of
      Just ts -> ts
      Nothing -> emptyTypeSet

class TypeSetifiable a where
    toTypeSet :: Symbol.SymbolTable -> TypeSet -> AttributeSet -> a -> P TypeSet

instance TypeSetifiable TypeOrAttributeId where
    toTypeSet symbol _ attSet toa =
        do ta <- Symbol.classifyTypeOrAttribute symbol toa
           case ta of
             Left ty -> return (singletonTypeSet ty)
             Right att -> return (getAttributeSet attSet att)

listToTypeSet :: Foldable l => (a -> P TypeSet) -> l a -> P TypeSet
listToTypeSet xToTys xs =
    foldlM inc emptyTypeSet xs
    where
      inc ts x = liftM (unionTypeSet ts) (xToTys x)

instance TypeSetifiable a => TypeSetifiable [SignedId a] where
    toTypeSet symbol allTys attSet xs =
        let (p,n) = partitionSignedIds xs in
        do posSet <- listToTypeSet (toTypeSet symbol allTys attSet) p
           negSet <- listToTypeSet (toTypeSet symbol allTys attSet) n
           differenceTypeSet posSet negSet

instance TypeSetifiable a => TypeSetifiable (NonEmptyList (SignedId a)) where
    toTypeSet symbol allTys attSet = toTypeSet symbol allTys attSet . toList

instance TypeSetifiable a => TypeSetifiable (NeverAllow a) where
    toTypeSet symbol allTys attSet na =
        case na of
          NeverAllow xs -> toTypeSet symbol allTys attSet xs
          NAStarTilde st -> toTypeSet symbol allTys attSet st

instance TypeSetifiable a => TypeSetifiable (StarTilde (SignedId a)) where
    toTypeSet symbol allTys attSet st =
        case st of
          Star -> return allTys
          Tilde xs ->
              do ctys <- toTypeSet symbol allTys attSet xs
                 differenceTypeSet allTys ctys

instance TypeSetifiable a => TypeSetifiable (Maybe a) where
    toTypeSet symbol allTys attSet = maybe (return emptyTypeSet)
                                           (toTypeSet symbol allTys attSet)

addAttributeSet :: AttributeId -> TypeId -> AttributeSet -> AttributeSet
addAttributeSet a t (as @ (AttributeSet m)) =
    let ts = getAttributeSet as a in
    let ts' = addTypeSet t ts in
    AttributeSet (Map.insert a ts' m)

addListAttributeSet :: Foldable l => 
                       l AttributeId -> TypeId -> AttributeSet -> AttributeSet
addListAttributeSet al t as =
    foldr add as al
    where
      add a z = addAttributeSet a t z

addTeRbacAttributeSet :: TeRbac -> AttributeSet -> AttributeSet
addTeRbacAttributeSet tr attSet =
    case tr of
      Type ty _ atts -> addListAttributeSet atts ty attSet
      TypeAttribute ty atts -> addListAttributeSet atts ty attSet
      _ -> attSet

fromTeRbacsAttributeSet :: [TeRbac] -> AttributeSet
fromTeRbacsAttributeSet = foldr addTeRbacAttributeSet emptyAttributeSet

newtype RoleTypeRel = RoleTypeRel (Map.Map RoleId TypeSet)
    deriving (Eq, Read, Show)

emptyRoleTypeRel :: RoleTypeRel
emptyRoleTypeRel = RoleTypeRel Map.empty

countRolesRoleTypeRel :: RoleTypeRel -> Int
countRolesRoleTypeRel (RoleTypeRel m) = Map.size m

countTypesRoleTypeRel :: RoleTypeRel -> Int
countTypesRoleTypeRel (RoleTypeRel m) =
    sizeTypeSet $ Map.fold unionTypeSet emptyTypeSet m

getRoleTypeRel :: RoleTypeRel -> RoleId -> TypeSet
getRoleTypeRel (RoleTypeRel m) r =
    case Map.lookup r m of
      Just ts -> ts
      Nothing -> emptyTypeSet

memberRoleTypeRel :: RoleId -> TypeId -> RoleTypeRel -> Bool
memberRoleTypeRel r t rtr = memberTypeSet t (getRoleTypeRel rtr r)

addRoleTypeRel :: RoleId -> TypeSet -> RoleTypeRel -> RoleTypeRel
addRoleTypeRel r ts (rtr @ (RoleTypeRel m)) =
    let ts' = getRoleTypeRel rtr r in
    let ts'' = unionTypeSet ts ts' in
    RoleTypeRel (Map.insert r ts'' m)

fromTeRbacsRoleTypeRel :: Symbol.SymbolTable -> TypeSet -> AttributeSet ->
                          [TeRbac] -> P RoleTypeRel
fromTeRbacsRoleTypeRel symbol allTys attSet =
    foldM fromTeRbac emptyRoleTypeRel
    where
      fromTeRbac rtr tr =
          case tr of
            Role r tas ->
                do ts <- toTypeSet symbol allTys attSet tas
                   return (addRoleTypeRel r ts rtr)
            Dominance _ -> throwError "UNSUPPORTED: dominance statement"
            _ -> return rtr

newtype UserRoleRel = UserRoleRel (Map.Map UserId RoleSet)
    deriving (Eq, Read, Show)

emptyUserRoleRel :: UserRoleRel
emptyUserRoleRel = UserRoleRel Map.empty

countUsersUserRoleRel :: UserRoleRel -> Int
countUsersUserRoleRel (UserRoleRel m) = Map.size m

countRolesUserRoleRel :: UserRoleRel -> Int
countRolesUserRoleRel (UserRoleRel m) =
    sizeRoleSet $ Map.fold unionRoleSet emptyRoleSet m

getUserRoleRel :: UserRoleRel -> UserId -> RoleSet
getUserRoleRel (UserRoleRel m) u =
    case Map.lookup u m of
      Just rs -> rs
      Nothing -> emptyRoleSet

memberUserRoleRel :: UserId -> RoleId -> UserRoleRel -> Bool
memberUserRoleRel u r urr = memberRoleSet r (getUserRoleRel urr u)

addUserRoleRel :: UserId -> RoleId -> UserRoleRel -> UserRoleRel
addUserRoleRel u r (urr @ (UserRoleRel m)) =
    let rs = getUserRoleRel urr u in
    let rs' = addRoleSet r rs in
    UserRoleRel (Map.insert u rs' m)

addListUserRoleRel :: Foldable l => 
                      UserId -> l RoleId -> UserRoleRel -> UserRoleRel
addListUserRoleRel u rs urr = foldr (addUserRoleRel u) urr rs

addUserUserRoleRel :: User -> UserRoleRel -> UserRoleRel
addUserUserRoleRel (User u rs) urr = addListUserRoleRel u rs urr

fromUsersUserRoleRel :: Foldable l => l User -> UserRoleRel
fromUsersUserRoleRel = foldr addUserUserRoleRel emptyUserRoleRel

newtype ClassPermissions =
    ClassPermissions (Map.Map ClassId Symbol.PermissionSym)
    deriving (Read, Show, Eq)

mkClassPermissions :: Foldable l => 
    Symbol.SymbolTable -> l ClassId -> Permissions -> P ClassPermissions
mkClassPermissions symbol cls perm =
    liftM ClassPermissions $ foldlM mkClPerm Map.empty cls
    where
      mkClPerm m cl =
          do ps <- Symbol.classPermissions symbol cl perm
             return (Map.insert cl ps m)

unionClassPermissions ::
    ClassPermissions -> ClassPermissions -> ClassPermissions
unionClassPermissions =
    \(ClassPermissions cp1) (ClassPermissions cp2) ->
        ClassPermissions (Map.unionWith Symbol.unionSymSet cp1 cp2)

disjointClassPermissions :: ClassPermissions -> ClassPermissions -> Bool
disjointClassPermissions =
    \(ClassPermissions cp1) (ClassPermissions cp2) ->
        allWithKeyMap (check cp2) cp1
    where
      check cp2 cl1 ps1 =
          case Map.lookup cl1 cp2 of
            Nothing -> True
            Just ps2 -> Symbol.disjointSymSet ps1 ps2

checkClassPermissions :: ClassPermissions -> ClassId -> PermissionId -> Bool
checkClassPermissions (ClassPermissions m) cl p =
    case Map.lookup cl m of
      Nothing -> False
      Just ps -> Symbol.memberSymSet p ps

newtype AllowTypes =
    AllowTypes (Map.Map TypeId (Map.Map TypeId ClassPermissions))
    deriving (Read, Show, Eq)

emptyAllowTypes :: AllowTypes
emptyAllowTypes = AllowTypes Map.empty

{-
nullAllowTypes :: AllowTypes -> Bool
nullAllowTypes (AllowTypes m) = Map.null m
-}

lookupAllowTypes :: AllowTypes -> TypeId -> TypeId -> Maybe ClassPermissions
lookupAllowTypes (AllowTypes m) sty tty =
    case Map.lookup sty m of
      Nothing -> Nothing
      Just ms -> Map.lookup tty ms

unionAllowTypes :: AllowTypes -> AllowTypes -> AllowTypes
unionAllowTypes =
    \(AllowTypes ar1) (AllowTypes ar2) -> AllowTypes (Map.unionWith unions ar1 ar2)
    where
      unions ars1 ars2 = Map.unionWith unionClassPermissions ars1 ars2

{-
intersectAllowTypes :: AllowTypes -> AllowTypes -> AllowTypes
intersectAllowTypes =
    \(AllowTypes ar1) (AllowTypes ar2) ->
        AllowTypes (Map.mapMaybeWithKey (inters ar2) ar1)
    where
      inters ar2 ty1 ars1 =
          case Map.lookup ty1 ar2 of
            Nothing -> Nothing
            Just ars2 ->
                let ars = Map.mapMaybeWithKey (interst ars2) ars1 in
                if Map.null ars then Nothing else Just ars

      interst ars2 ty1 arst1 =
          case Map.lookup ty1 ars2 of
            Nothing -> Nothing
            Just arst2 ->
                let arst = Map.mapMaybeWithKey (interstc arst2) arst1 in
                if Map.null arst then Nothing else Just arst

      interstc arst2 cl1 arstc1 =
          case Map.lookup cl1 arst2 of
            Nothing -> Nothing
            Just arstc2 ->
                let arstc = Symbol.intersectSymSet arstc1 arstc2 in
                if Symbol.nullSymSet arstc then Nothing else Just arstc
-}

{-
disjointAllowTypes :: AllowTypes -> AllowTypes -> Bool
disjointAllowTypes =
    \(AllowTypes ar1) (AllowTypes ar2) -> not (anyWithKeyMap (search ar2) ar1)
    where
      search ar2 sty1 ars1 =
          case Map.lookup sty1 ar2 of
            Nothing -> False
            Just ars2 -> anyWithKeyMap (searchs ars1) ars2

      searchs ars2 tty1 cp1 =
          case Map.lookup tty1 ars2 of
            Nothing -> False
            Just cp2 -> not (disjointClassPermissions cp1 cp2)
-}

singletonAllowTypes :: TypeId -> TypeId -> ClassPermissions -> AllowTypes
singletonAllowTypes sty tty clPerms =
    AllowTypes (Map.singleton sty (Map.singleton tty clPerms))

addAllowTypes :: TypeId -> TypeId -> ClassPermissions -> AllowTypes -> AllowTypes
addAllowTypes sty tty clPerms ar =
    unionAllowTypes (singletonAllowTypes sty tty clPerms) ar

mkAllowTypes :: TypeSet -> Bool -> TypeSet -> ClassPermissions -> AllowTypes
mkAllowTypes stys self ttys clPerms =
    foldSelfTypeSet add emptyAllowTypes stys self ttys
    where
      add sty tty ar = addAllowTypes sty tty clPerms ar

checkAllowTypes ::
    AllowTypes -> TypeId -> TypeId -> ClassId -> PermissionId -> Bool
checkAllowTypes ar sty tty cl p =
    case lookupAllowTypes ar sty tty of
      Nothing -> False
      Just cp -> checkClassPermissions cp cl p

newtype Environment = Environment (Map.Map BoolId Bool)
    deriving (Eq, Read, Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment Map.empty

sizeEnvironment :: Environment -> Int
sizeEnvironment (Environment m) = Map.size m

addEnvironment :: BoolId -> Bool -> Environment -> Environment
addEnvironment i v (Environment m) = Environment (Map.insert i v m)

getEnvironment :: Environment -> BoolId -> P Bool
getEnvironment (Environment m) b =
    case Map.lookup b m of
      Just v -> return v
      Nothing -> throwError $ "getEnvironment: no such boolean " ++ show b

fromTeRbacsEnvironment :: [TeRbac] -> Environment
fromTeRbacsEnvironment =
    \trs -> foldr fromTeRbac emptyEnvironment trs
    where
      fromTeRbac tr env =
          case tr of
            BoolDef i v -> addEnvironment i v env
            _ -> env

condExprEnvironment :: Environment -> CondExpr -> P Bool
condExprEnvironment env =
    ece
    where
      ece (Not ce) = liftM not $ ece ce
      ece (Op ce1 And ce2) = return (&&) `ap` ece ce1 `ap` ece ce2
      ece (Op ce1 Or ce2) = return (||) `ap` ece ce1 `ap` ece ce2
      ece (Op ce1 Xor ce2) = return (/=) `ap` ece ce1 `ap` ece ce2
      ece (Op ce1 Equals ce2) = return (==) `ap` ece ce1 `ap` ece ce2
      ece (Op ce1 Notequal ce2) = return (/=) `ap` ece ce1 `ap` ece ce2
      ece (Var b) = getEnvironment env b

data Conditionals =
     Conditionals
     {allowRel :: AllowTypes,
      subConditionals :: [Conditional]}
    deriving (Read, Show, Eq)

data Conditional = Conditional CondExpr Conditionals Conditionals
    deriving (Read, Show, Eq)

emptyConditionals :: Conditionals
emptyConditionals =
    Conditionals
    {allowRel = emptyAllowTypes,
     subConditionals = []}

addSubConditional :: Conditional -> Conditionals -> Conditionals
addSubConditional subCond conds =
    let Conditionals {subConditionals = subConds} = conds in
    conds {subConditionals = subCond : subConds}

fromTeRbacsConditionals ::
    Symbol.SymbolTable -> TypeSet -> AttributeSet -> [TeRbac] -> P Conditionals
fromTeRbacsConditionals symbol allTys attSet =
    \trs -> foldM fromTeRbac emptyConditionals trs
    where
      fromTeRbac conds tr =
          case tr of
            Stmt s -> fromStmt conds s
            CondStmt ce ts es ->
                do tc <- foldlM fromRequireStmt emptyConditionals ts
                   ec <- foldlM fromRequireStmt emptyConditionals es
                   return (addSubConditional (Conditional ce tc ec) conds)
            _ -> return conds

      fromRequireStmt conds rs =
          case rs of
            RequireStmt s -> fromStmt conds s
            Require _ -> throwError "fromRequireStmt: extant require blocks"

      fromStmt conds stmt =
          case stmt of
            TeAvTab ad (SourceTarget {sourceTypes = st,
                                      targetTypes = tt,
                                      targetClasses = cls}) ps ->
              do allowed <- isAllowAllowDeny ad
                 if not allowed then return conds else
                   do stys <- toTypeSet symbol allTys attSet st
                      (self,tt') <- destListSignedSelf tt
                      ttys <- toTypeSet symbol allTys attSet tt'
                      clPerms <- mkClassPermissions symbol cls ps
                      ar <- returnHyperStrict $ 
                            mkAllowTypes stys self ttys clPerms
                      let ar' = unionAllowTypes ar (allowRel conds)
                      return (conds {allowRel = ar'})
            _ -> return conds

sizeConditionals :: Conditionals -> Int
sizeConditionals = length . subConditionals

checkConditionals ::
    Environment -> TypeId -> TypeId -> ClassId -> PermissionId ->
    Conditionals -> P Bool
checkConditionals env sty tty cl p =
    \conds ->
        liftM (checkAllowTypes (allowRel conds) sty tty cl p ||)
        (checkSubs (subConditionals conds))
    where
      checkSubs [] = return False
      checkSubs (subCond : subConds) =
          return (||) `ap` checkConditional env sty tty cl p subCond `ap`
          checkSubs subConds

checkConditional ::
    Environment -> TypeId -> TypeId -> ClassId -> PermissionId ->
    Conditional -> P Bool
checkConditional env sty tty cl p (Conditional ce t e) =
    do b <- condExprEnvironment env ce
       checkConditionals env sty tty cl p (if b then t else e)

flattenAllowTypesConditionals :: Conditionals -> AllowTypes
flattenAllowTypesConditionals conds =
    foldr flattenAllowTypesConditional (allowRel conds) (subConditionals conds)

flattenAllowTypesConditional :: Conditional -> AllowTypes -> AllowTypes
flattenAllowTypesConditional (Conditional _ t e) ar =
    let tar = flattenAllowTypesConditionals t in
    let ear = flattenAllowTypesConditionals e in
    unionAllowTypes ar (unionAllowTypes tar ear)

type ConstraintFn = SecurityContext -> SecurityContext -> Bool

newtype Constrains =
    Constrains (Map.Map ClassId (Map.Map PermissionId ConstraintFn))

emptyConstrains :: Constrains
emptyConstrains = Constrains Map.empty

sizeConstrains :: Constrains -> Int
sizeConstrains (Constrains m) = Map.fold (\mc n -> Map.size mc + n) 0 m

lookupConstrains :: Constrains -> ClassId -> PermissionId -> Maybe ConstraintFn
lookupConstrains (Constrains m) cl p =
    case Map.lookup cl m of
      Just mc -> Map.lookup p mc
      Nothing -> Nothing

insertConstrains :: ClassId -> PermissionId -> ConstraintFn ->
                    Constrains -> Constrains
insertConstrains =
    \cl p cf (Constrains m) ->
        Constrains (Map.insert cl (ins p cf (Map.lookup cl m)) m)
    where
      ins p cf mco =
          case mco of
            Nothing -> Map.singleton p cf
            Just mc -> Map.insert p cf mc

addConstraintConstrains ::
    Symbol.SymbolTable -> TypeSet -> AttributeSet ->
    Constraint -> Constrains -> P Constrains
addConstraintConstrains symbol allTys attSet =
    \con constr ->
        case con of
          Constrain cls ps ce ->
              do cf <- ece ce
                 return (foldr (addc ps cf) constr cls)
          ValidateTrans _ _ ->
              throwError "UNSUPPORTED: validatetrans constraint"

    where
      addc ps cf cl constr = foldr (addcp cl cf) constr ps

      addcp cl cf p constr =
          let cf' = case lookupConstrains constr cl p of
                      Nothing -> cf
                      Just f -> and2 cf f in
          insertConstrains cl p cf' constr

      ece (ConstraintExprPrim p) = ecep p
      ece (CNot ce) = not2 `liftM` ece ce
      ece (COp ce1 CAnd ce2) = return and2 `ap` ece ce1 `ap` ece ce2
      ece (COp ce1 COr ce2) = return or2 `ap` ece ce1 `ap` ece ce2

      ecep (CUsers e) = return (ecen e (userFn (==)))
      ecep (CRoles (CEqOp e)) = return (ecen e (roleFn (==)))
      ecep (CRoles Dom) = throwError "UNSUPPORTED: dom role constraint"
      ecep (CRoles DomBy) = throwError "UNSUPPORTED: domby role constraint"
      ecep (CRoles InComp) = throwError "UNSUPPORTED: incomp role constraint"
      ecep (CTypes e) = return (ecen e (typeFn (==)))
      ecep (CUserSet c e ul) =
          (ecen e . userFn) `liftM` contextFn c (flip elem (toList ul))
      ecep (CRoleSet c e rl) =
          (ecen e . roleFn) `liftM` contextFn c (flip elem (toList rl))
      ecep (CTypeSet c e tal) =
          do ts <- listToTypeSet (toTypeSet symbol allTys attSet) tal
             (ecen e . typeFn) `liftM` contextFn c (flip memberTypeSet ts)
{- Equivalent to:
          (ecen e . typeFn) `liftM`
          ((contextFn c . flip memberTypeSet) =<<
           listToTypeSet (toTypeSet symbol allTys attSet) tal)
-}

      ecen CEquals f = f
      ecen CNotequal f = not2 f

      not2 f x y = not (f x y)

      and2 f g x y = f x y && g x y

      or2 f g x y = f x y || g x y

      contextFn C1 f = return (\x _ -> f x)
      contextFn C2 f = return (\_ x -> f x)
      contextFn C3 _ = throwError "bad context index C3"

      userFn f (SecurityContext u1 _ _) (SecurityContext u2 _ _) = f u1 u2

      roleFn f (SecurityContext _ r1 _) (SecurityContext _ r2 _) = f r1 r2

      typeFn f (SecurityContext _ _ t1) (SecurityContext _ _ t2) = f t1 t2

fromConstraintsConstrains ::
    Symbol.SymbolTable -> TypeSet -> AttributeSet ->
    [Constraint] -> P Constrains
fromConstraintsConstrains symbol allTys attSet =
    foldM (flip (addConstraintConstrains symbol allTys attSet)) emptyConstrains

checkConstrains :: Constrains -> ClassId -> PermissionId -> ConstraintFn
checkConstrains constrs cl p subj obj =
    case lookupConstrains constrs cl p of
      Nothing -> True
      Just cf -> cf subj obj

data Authorize =
    Authorize
    {attributeSet :: AttributeSet,
     roleTypeRel :: RoleTypeRel,
     userRoleRel :: UserRoleRel,
     environment :: Environment,
     conditionals :: Conditionals,
     constrains :: Constrains}

empty :: Authorize
empty =
    Authorize
    {attributeSet = emptyAttributeSet,
     roleTypeRel = emptyRoleTypeRel,
     userRoleRel = emptyUserRoleRel,
     environment = emptyEnvironment,
     conditionals = emptyConditionals,
     constrains = emptyConstrains}

setBool :: BoolId -> Bool -> Authorize -> Authorize
setBool i v auth = auth {environment = addEnvironment i v (environment auth)}

summarize :: Authorize -> String
summarize (Authorize {attributeSet = attSet,
                      roleTypeRel = rtr,
                      userRoleRel = urr,
                      environment = env,
                      conditionals = conds,
                      constrains = constrs}) =
    "attributes: " ++ show (sizeAttributeSet attSet) ++
    " (containing " ++ show (countUniqueTypesAttributeSet attSet) ++
    " unique types)\n" ++
    "role type relation: " ++ show (countRolesRoleTypeRel rtr) ++
    " roles <--> " ++ show (countTypesRoleTypeRel rtr) ++ " types\n" ++
    "user role relation: " ++ show (countUsersUserRoleRel urr) ++
    " users <--> " ++ show (countRolesUserRoleRel urr) ++ " roles\n" ++
    "boolean variables: " ++ show (sizeEnvironment env) ++ "\n" ++
    "conditionals: " ++ show (sizeConditionals conds) ++ "\n" ++
    "constraints: " ++ show (sizeConstrains constrs) ++ "\n"

data Result = Result {allow :: Bool {-, audit :: Bool-}}
    deriving (Read, Show)

validSecurityContext :: Authorize -> SecurityContext -> Bool
validSecurityContext auth (SecurityContext u r t) =
    let Authorize {roleTypeRel = rtr, userRoleRel = urr} = auth in
    memberRoleTypeRel r t rtr && memberUserRoleRel u r urr

allowedTypeAccess :: Authorize ->
                     TypeId -> TypeId -> ClassId -> PermissionId ->
                     P Result
allowedTypeAccess auth sty tty cl p =
    let Authorize {environment = env, conditionals = conds} = auth in
    do allowed <- checkConditionals env sty tty cl p conds
       return (Result {allow = allowed})

unconstrainedAccess :: Authorize ->
                       SecurityContext -> SecurityContext ->
                       ClassId -> PermissionId ->
                       Result
unconstrainedAccess auth subj obj cl p =
    let Authorize {constrains = constrs} = auth in
    Result {allow = checkConstrains constrs cl p subj obj}

allowedAccess :: Authorize ->
                 SecurityContext -> SecurityContext ->
                 ClassId -> PermissionId ->
                 P Result
allowedAccess auth subj obj cl p =
    let SecurityContext _ _ sty = subj in
    let SecurityContext _ _ tty = obj in
    do result <- allowedTypeAccess auth sty tty cl p
       let result' = unconstrainedAccess auth subj obj cl p
       return (Result {allow = allow result && allow result'})

validateSecurityContext :: Symbol.SymbolTable -> Authorize ->
                           SecurityContext ->
                           P (Maybe SecurityContext)
validateSecurityContext symbol auth sc =
    do sc' <- Symbol.normalize symbol sc
       return (if validSecurityContext auth sc' then Just sc' else Nothing)

validateSecurityContexts :: Symbol.SymbolTable -> Authorize ->
                            SecurityContext -> SecurityContext ->
                            P (Maybe (SecurityContext,SecurityContext))
validateSecurityContexts symbol auth sc1 sc2 =
    do osc1 <- validateSecurityContext symbol auth sc1
       case osc1 of
         Nothing -> return Nothing
         Just sc1' ->
             do osc2 <- validateSecurityContext symbol auth sc2
                case osc2 of
                  Nothing -> return Nothing
                  Just sc2' -> return (Just (sc1',sc2'))

isAuthorizedP :: Symbol.SymbolTable -> Authorize ->
                 SecurityContext -> SecurityContext ->
                 ClassId -> PermissionId ->
                 P Result
isAuthorizedP symbol auth subj obj cl p =
    do scs <- validateSecurityContexts symbol auth subj obj
       case scs of
         Nothing -> return (Result {allow = False})
         Just (subj',obj') -> allowedAccess auth subj' obj' cl p

isAuthorized :: Symbol.SymbolTable -> Authorize ->
                SecurityContext -> SecurityContext ->
                ClassId -> PermissionId ->
                Either String Result
isAuthorized symbol auth subj obj cl p =
    runP $ isAuthorizedP symbol auth subj obj cl p

accessVectorP :: Symbol.SymbolTable -> Authorize ->
                 SecurityContext -> SecurityContext ->
                 ClassId ->
                 P Symbol.PermissionSym
accessVectorP symbol auth subj obj cl =
    do scs <- validateSecurityContexts symbol auth subj obj
       case scs of
         Nothing -> return Symbol.emptySymSet
         Just (subj',obj') ->
             do perms <- Symbol.allClassPermissions symbol cl
                Symbol.filterSymSetM isAuth perms
             where
               isAuth p = liftM allow $ allowedAccess auth subj' obj' cl p

accessVector :: Symbol.SymbolTable -> Authorize ->
                SecurityContext -> SecurityContext ->
                ClassId ->
                Either String Symbol.PermissionSym
accessVector symbol auth subj obj cl =
    runP $ accessVectorP symbol auth subj obj cl

checkTeRbacAssertion ::
    Symbol.SymbolTable -> TypeSet -> Authorize -> AllowTypes -> TeRbac -> P ()
checkTeRbacAssertion symbol allTys auth ar tr =
    case tr of
      TeNeverAllow (SourceTarget {sourceTypes = st,
                                  targetTypes = tt,
                                  targetClasses = cls}) ps ->
          do {-trace ("checking \"" ++ prettyPrint tr ++ "\"") (return ())-}
             let Authorize {attributeSet = attSet} = auth
             stys <- toTypeSet symbol allTys attSet st
             (self,mtt') <- destNeverAllowSelf tt
             ttys <- case mtt' of Just tt' -> toTypeSet symbol allTys attSet tt'
                                  Nothing -> return emptyTypeSet
             clPerms <- mkClassPermissions symbol cls ps
             {-trace ("stys: " ++ show (sizeTypeSet stys)) (return ())-}
             {-trace ("self: " ++ show self) (return ())-}
             {-trace ("ttys: " ++ show (sizeTypeSet ttys)) (return ())-}
             if allSelfTypeSet (disjoint clPerms) stys self ttys
               then return ()
               else throwError "failed neverallow check"
      _ -> return ()
      where
        disjoint clPerms sty tty =
            case lookupAllowTypes ar sty tty of
              Nothing -> True
              Just cp -> disjointClassPermissions clPerms cp

fromPolicy :: Policy -> Symbol.SymbolTable -> P Authorize
fromPolicy policy symbol =
    let Policy {classes = _,
                initialSids = _,
                commonPerms = _,
                avPerms = _,
                teRbacs = trs,
                users = us,
                constraints = cons} = policy in
{-
                sidContexts = ss,
                fileSystemUses = fsus,
                genFileSystemContexts = gfscs,
                portContexts = pcs,
                netInterfaceContexts = nics,
                nodeContexts = ncs} = policy in
-}
    let allTys = allTypes symbol in
    do attSet <- returnHyperStrict (fromTeRbacsAttributeSet trs)
       rtr <- returnHyperStrict =<< 
              fromTeRbacsRoleTypeRel symbol allTys attSet trs
       urr <- returnHyperStrict (fromUsersUserRoleRel us)
       env <- returnHyperStrict (fromTeRbacsEnvironment trs)
       conds <- returnHyperStrict =<< 
                fromTeRbacsConditionals symbol allTys attSet trs
       constrs <- fromConstraintsConstrains symbol allTys attSet cons
       let auth = Authorize {attributeSet = attSet,
                             roleTypeRel = rtr,
                             userRoleRel = urr,
                             environment = env,
                             conditionals = conds,
                             constrains = constrs}
       let ar = flattenAllowTypesConditionals conds
       {-trace "start checking assertions" (return ())-}
       mapM_ (checkTeRbacAssertion symbol allTys auth ar) trs
       {-trace "finished checking assertions" (return ())-}
       return auth

build :: Policy -> Symbol.SymbolTable -> Either String Authorize
build policy symbol = runP $ fromPolicy policy symbol
