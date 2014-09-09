{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -XCPP #-}
{- |
Module      : $Header$
Description : SELinux kind-checking implementation
Copyright   : (c) Galois, Inc.

Implementation of SELinux kind-checking algorithms.
-}

module SCD.M4.KindCheck
       ( KC
       , KindCheckOptions(..)
       , defaultKindCheckOptions
       , ignoreErrorsKindCheckOptions
       , Kind(..)
       , ParameterKind(..)
       , ParameterIndex(..)
       , HasKind(..)
       , Fragment(..)
       , KindMap
       , M4Info(..)
       , KindMaps(..)
       , InterfaceEnv(..)
       , kcPolicy
       , kcBaseEnv
       , KindInfo(..)
       , ClassPermissionMap
       , simpleKindSet
       , simpleParameterKind
       , simpleParameterInfo
       , normalizeId
       , IdXrefMap
       , T, P, Env
       ) where

-- import Debug.Trace ( trace )

import SCD.M4.Kind
import SCD.M4.KindInfo

import SCD.M4.Errors(Error(..), nubError)

import SCD.M4.Syntax(IfdefId, LevelId, MlsRange(..), M4Id,
  LayerModule, Policy(..), PolicyModule(..), Interface(..),
  InterfaceElement(..), InterfaceType(..),
  InterfaceDoc(..), Parameter, Stmt(..), Require(..), GenContext(..),
  FileContext(..), FileContexts(..), Implementation(..),
  SupportDef(..), ClassPermissionDef(..),
  GlobalBoolean(..), RefPolicyWarnLine(..), ModuleConfSetting(..),
  ModuleConf(..), required, IfdefDecl(..))
import SCD.M4.Util ( implementationId, elementId )

import qualified SCD.M4.Syntax as Syntax

import SCD.SELinux.Syntax(Identifier, IsIdentifier(..), mkId,
  ClassId, CommonId, RoleId, PermissionId, TypeId, AllowDeny(..),
  AttributeId, TypeOrAttributeId, UserId, BoolId, Sid, NetInterfaceId,
  FileSystemId, CondExpr(..), SignedId(..), signedId2Id, Sign(..),
  NeverAllow(..), StarTilde(..), Self(..), Permissions(..),
  SourceTarget(..), SidContext(..), FileSystemUse(..),
  GenFileSystemContext(..), PortContext(..), NetInterfaceContext(..),
  NodeContext(..), FilePath(..), AvPerm(..), CommonPerm(..))

import SCD.M4.Options(Options(..))

import SCD.M4.Dependencies(sortCalls, callGraphPolicy, callGraphPolicyModules)

import SCD.M4.PShow(PShow)

import Prelude hiding (FilePath, mapM_, lookup)

import Data.NonEmptyList(NonEmptyList)
import Data.Maybe(isNothing, isJust, fromJust, fromMaybe)

import Data.Map(Map, union, difference, elems, lookup, member, assocs,
  mapWithKey, insert, insertWith, fromListWith, findWithDefault,
  fromList, keys, keysSet, filterWithKey, findMax)

import qualified Data.MapSet as MapSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Graph(SCC(..), graphFromEdges, dfs)
import Data.Foldable(Foldable, mapM_, toList, foldlM)

import Data.List   ( genericLength, partition )
import Data.Monoid(Monoid)

import Control.Arrow(first)
import Control.Monad(when, zipWithM_)
import Control.Monad.Identity(Identity, runIdentity)
import Control.Monad.State(StateT, runStateT, MonadState, gets, modify)
import Control.Monad.Writer(WriterT, runWriterT, MonadWriter, tell, censor)
import Control.Monad.Reader(ReaderT, runReaderT, MonadReader, asks, local)

---------------------------

data KindCheckOptions
 = KindCheckOptions
     { emitErrors :: Bool
     }
  deriving Show

defaultKindCheckOptions :: KindCheckOptions
defaultKindCheckOptions
 = KindCheckOptions{emitErrors = True}

ignoreErrorsKindCheckOptions :: KindCheckOptions
ignoreErrorsKindCheckOptions
 = defaultKindCheckOptions{emitErrors=False}

newtype P r w s a = P (ReaderT r (StateT s (WriterT w Identity)) a)
    deriving (Monad,
#ifndef __HADDOCK__
              MonadState s, MonadWriter w, MonadReader r,
#endif
              Functor)

runP :: Monoid w => P r w s a -> s -> r -> ((a,s),w)
runP (P p) s r = (runIdentity . runWriterT . flip runStateT s . flip runReaderT r) p

type T a = P Env [Error] KindInfo a

runT :: Options -> KindCheckOptions -> T a -> ((a,KindInfo),[Error])
runT os kcOpts t
  = getRes $ compRes $ runP t emptyKindInfo iEnv
  where
    getRes (a,errs)
     | not (emitErrors kcOpts) = (length errs) `seq` (a,[])
     | otherwise = (a,errs)

    compRes (a, errs) = (a, nubError errs)

    iEnv =
      Env{ base            = Map.empty
         , moduleConfigMap = Map.empty
         , source          = False
         , options         = os
	 , kcOptions       = kcOpts
	 }



type BaseEnv = KindMap

data Env = Env{ base            :: BaseEnv
              , moduleConfigMap :: ModuleConfigMap
              , source          :: Bool
              , options         :: Options
	      , kcOptions       :: KindCheckOptions
              }
  deriving Show

type ModuleConfigMap = Map.Map Identifier ModuleConfSetting

class KC a where
  kc :: a -> T ()

instance KC a => KC [a] where
  kc = mapM_ kc

instance KC a => KC (NonEmptyList a) where
  kc = mapM_ kc

instance KC a => KC (Maybe a) where
  kc (Just a) = kc a
  kc Nothing  = return ()

{-
Kind checking a list of modules
0. get initial environment
   support: file_patterns.spt, ipc_patterns.spt, misc_patterns.spt
            obj_perm_sets: how to type, when to expand?
   special: get env from policy_module def in loadable_module.spt
            mls_mcs_macros.spt ?
1. sort calls
2. kind-check definitions (templates and interfaces)
3. kind-check implementations and filecontexts
-}

{-

== Checking a list of modules given a policy ==

Arguments: list of implementation files.

* For each implementation file, parse corresponding interface
  file and file-context file.
* Topologically sort the argument modules.

* Form definition map from argument modules + policy
* Kind check cs, keep only errors for argument modules (?)
* Kind check implementation of argument modules

Potential problems:

  + This will not capture duplicate module identifiers
    between argument modules and unreachable policy modules,
    or between argument interfaces and unreachable policy
    interfaces.

  + some illegal cross references will be reported as
    undefined identifers instead.
-}


-- | Performs kind-checking on a list of policy modules given a
-- SELinux policy.  If the list of policy modules is empty, kind check
-- all the modules in the policy instead.

kcPolicy :: Options
         -> KindCheckOptions
	 -> Policy
	 -> [PolicyModule]
	 -> (KindInfo, [Error])
kcPolicy os kcOpts p pms = (s,e) where
    ((_,s),e) = runT os kcOpts checkPolicy
    checkPolicy =
      case pms of
        [] -> kcPolicy' p{policyModules=[]} (policyModules p)
	_  -> kcPolicy' p pms

kcPolicy' :: Policy -> [PolicyModule] -> T ()
kcPolicy' p pms = do
  (_,be) <- baseEnv p
    -- build up a (ModuleName -> ModuleConfSetting) map.
  mcm <- (buildModuleConfigMap p :: T ModuleConfigMap)
  reportUnknownModules mcm p

  bi <- gets (outputMap . kindMaps . interfaceEnv)
    -- ..and a mapping from ifdef'ed values to enable flags.
  ifdefsMap <- buildIdMap (\ (IfdefDecl i v) -> (toId i, v)) (ifdefDecls p)
  modify $ \s -> s{ m4Env = m4Env s `union` fmap M4Ifdef ifdefsMap }
  local (\e -> e{ base = be, moduleConfigMap = mcm }) $ do
   groups <- sortModules p pms
   let apms = pms ++ policyModules p
   let dms  = definitionMap p{ policyModules = apms}

   let mds  = map fst (filter ((>1).length.snd) (assocs dms))
   when (not (null mds)) $
      kcError $ DuplicateDefinitions mds

   let dm = fmap head dms
   mapM_ (either kc kc . flip (findWithDefault (error "kcPolicyModules")) dm)
         groups

   mapM_ kcImplementation pms
   modify $ \ki -> ki{ xrefs = xrefOutputMaps ki apms }
   mapM_ (checkXrefDependencies bi) pms

kcBaseEnv :: Options -> KindCheckOptions -> Policy -> ((ClassPermissionMap,BaseEnv), [Error])
kcBaseEnv os kcOpts p = (a,e)
  where ((a,_),e) = runT os kcOpts (baseEnv p)

-- | Check for duplicate definitions in classes, sids, permissions,
-- commons, and booleans.  Return base environment, which is globally
-- visible.

baseEnv :: Policy -> T (ClassPermissionMap, BaseEnv)
baseEnv p = do
  mapM_ kcAddOrigin (kernelClasses p)
  mapM_ kcAddOrigin (userClasses p)
  mapM_ kcAddOrigin (initialSids p)
  com <- foldlM commonPermissionMapEnv Map.empty (commonPerms p)
  clm <- foldlM (addAv com) Map.empty (Syntax.avPerms p)
  mapM_ (checkMissingAv clm) (kernelClasses p++userClasses p)
  genClassPerms clm
  mapM_ classPermissionDefEnv (classPermissionDefs p)
  mapM_ kcAddOrigin [b | GlobalBoolean _ _ b _ <- globalBooleans p]
  return (clm, computeBaseEnv p clm)

sortModules :: Policy -> [PolicyModule] -> T [M4Id]
sortModules p pms = do
   when (not (null cyclic_groups)) $
         kcError $ MutuallyRecursive cyclic_groups
   return acyclic_groups
 where
  (cyclic_groups, acyclic_groups) =
     (\ (x,y) -> (map strip x, concatMap strip y)) $
        partition isCyclicSCC css
   where
    strip (AcyclicSCC x) = [x]
    strip (CyclicSCC x)  = x

  css = sortCalls subcg
  subcg
        -- check everything, including support definitions
   | null (policyModules p) = cg
   | otherwise = only_these_mods

  only_these_mods =
   fromList [ ces | (ces,_,_) <- concatMap (map fromV . toList) sorted]

  cg     = callGraphPolicy p `MapSet.union` pmscg
  pmscg  = callGraphPolicyModules pms
  sorted = dfs g (map (fromJust . toV) (keys pmscg))
  (g,fromV,toV) =
    graphFromEdges [ ((c,es),c,map Left (toList es))
		   |  (c,es) <- assocs cg ]

isCyclicSCC :: SCC a -> Bool
isCyclicSCC CyclicSCC{} = True
isCyclicSCC _ = False

-- | @buildModuleConfigMap@ constructs a mapping from modules mentioned in
-- the toplevel module config file to their kinds (base/module/..)
buildModuleConfigMap :: Policy -> T ModuleConfigMap
buildModuleConfigMap p
 = buildIdMap (\ (ModuleConf i s) -> (toId i,s)) (modulesConf p)

-- | @buildIdMap f ls@ is a local utility functions for building up
-- new kinds of maps. Lifted into a monad so that we can track duplicates
-- warnings\/errors as we go along.
buildIdMap :: (a -> (Identifier,b)) -> [a] -> T (Map.Map Identifier b)
buildIdMap f ls =
  foldlM (\m v -> case f v of { (i,x) -> addId m i x})
         Map.empty
	 ls

reportUnknownModules :: ModuleConfigMap -> Policy -> T ()
reportUnknownModules mcm p
  | not (Set.null um) = kcError $ UnknownModuleConfigNames (Set.map fromId um)
  | otherwise         = return ()
 where
   -- report any of the modules in the map that don't
   -- have a corresponding composite policy module
   -- (i.e., the triple of interface/templates, .te, .fc)
   -- defined.
  um = (Set.\\)
          (keysSet mcm)
          (Set.fromList $
	     map (mkId . baseName)
                 (policyModules p))

type CommonPermissionMap = Map CommonId (Set PermissionId)
type ClassPermissionMap = Map ClassId (Set PermissionId)

commonPermissionMapEnv :: CommonPermissionMap -> CommonPerm -> T CommonPermissionMap
commonPermissionMapEnv com (CommonPerm c ps)
 | c `member` com = kcError (DuplicateCommonDef c (findOrig c com)) >> return com
 | otherwise = do
   ps' <- foldlM (addPerm (flip DuplicateCommonPermission c)) Set.empty (toList ps)
   return (insert c ps' com)

addAv :: CommonPermissionMap
      -> ClassPermissionMap
      -> AvPerm
      -> T ClassPermissionMap
addAv com clm (AvPermClass c eps)
 | (c `member` clm) = do
   kcError (DuplicateAccessVectorDefinition c (findOrig c clm))
   return clm
 | otherwise = do
   (cps,nps) <- findPermissionSet eps
     -- add permission IDs to permission set, signalling
     -- any duplicates encountered.
   ps <- foldlM (addPerm (flip DuplicateClassPermission c)) cps nps
   return (insert c ps clm)
 where
   -- an AV permission class bundles a set of permissionIDs with a name.
  findPermissionSet (Left pids) = return (Set.empty, toList pids)
  findPermissionSet (Right (commonId, pids)) =
     case lookup commonId com of
       Nothing -> do
         kcError (UndefinedCommonId commonId)
	 return (Set.empty, pids)
       Just cPermSet -> return (cPermSet, pids)

addPerm :: (PermissionId -> Error)
        -> Set PermissionId
	-> PermissionId
	-> T (Set PermissionId)
addPerm dupError cps p = do
  when (Set.member p cps) (kcError (dupError p))
  ks <- lookupKind (toId p)
  let isPermKind = Set.member PermissionKind ks
  when (not isPermKind) (kcAddOrigin p)
  return (Set.insert p cps)

-- TODO: use addId in more places.
addId :: IsIdentifier i => Map i v -> i -> v -> T (Map i v)
addId m i v = do
  when (i `member` m) $
    kcError $ DuplicateIdentifier (toId i) (toId (findOrig i m))
  return (insert i v m)

checkMissingAv :: ClassPermissionMap -> ClassId -> T ()
checkMissingAv clm c =
  when (not (c `member` clm)) $
    kcError $ MissingAccessVectorDefinition c


genClassPerms :: ClassPermissionMap -> T ()
genClassPerms clm = mapM_ gen (assocs clm)
 where
  gen (c,ps) =
   addM4Env i (M4IdSet (Set.singleton PermissionKind)
                       (Set.map toId ps) Nothing)
   where i = mkId ("all_"++idString c++"_perms")


classPermissionDefEnv :: ClassPermissionDef -> T ()
classPermissionDefEnv (ClassPermissionDef i s ow) = do
  let lo (ps,ks) p = do
        ks' <- lookupKind p
        when (Set.null ks') $
          kcError $ UndefinedIdentifier p
        when (Set.member p ps) $
          kcError $ DuplicateOccurrences i p
        _ <- checkNotMacro p
        return (Set.insert p ps, Set.union ks ks')
  (ps,ks) <- foldlM lo (Set.empty, Set.empty) =<< expandSets Nothing s
  let i' = toId i
  addM4Env i' (M4IdSet ks ps ow)

expandSets  :: (PShow i, IsIdentifier i, Foldable l) => Maybe Kind -> l i -> T (Set i)
expandSets k is = Set.unions `fmap` mapM (expandSet k) (toList is)

expandSet :: (PShow i, IsIdentifier i) => Maybe Kind -> i -> T (Set i)
expandSet mk i = do
  let i' = toId i
  mi <- lookupM4Env i'
  case mi of
    Just (M4IdSet ks is ow) -> do
      checkRefPolicyWarnMacro i' ow
      case mk of
        Nothing -> return ()
        Just k ->
          when (not (k `Set.member` ks)) $
            kcError $ KindMismatch k ks (toId i)
      return (Set.map fromId is)
    _ -> do return (Set.singleton i)

checkRefPolicyWarnMacro :: Identifier -> (Maybe RefPolicyWarnLine) -> T ()
checkRefPolicyWarnMacro i (Just (RefPolicyWarnLine w)) = kcError $ RefPolicyWarnCall i [w]
checkRefPolicyWarnMacro _ Nothing                      = return ()

checkNotMacro :: (PShow i, IsIdentifier i) => i -> T Bool
checkNotMacro i = do
  let i' = toId i
  mi <- lookupM4Env i'
  case mi of
    Nothing -> return False
    Just _ -> do
      Just oi <- lookupOrigId i'
      kcError $ IllegalMacroUse (toId i) oi
      return True

computeBaseEnv :: Policy -> ClassPermissionMap -> BaseEnv
computeBaseEnv p clm =
  fromList (concat
    [mkEnv c ClassKind:
     [mkEnv p' PermissionKind | p' <- toList (MapSet.lookup c clm)]
    | c <- kernelClasses p])
  `MapSet.union` systemBaseEnv
  where
   mkEnv i k = (i', Set.singleton (posKind i' k))
    where i' = toId i

   systemBaseEnv :: BaseEnv
   systemBaseEnv = fromList $
      map (uncurry mkEnv . first (mkId::String->Identifier))
          [ -- mls_mcs_macros:
            ("mcs_systemhigh", LevelKind)
          , ("s0", LevelKind)
          , ("mls_systemhigh", LevelKind)
            -- roles: TODO: parse support/users instead.
          , ("system_r", RoleKind)
          , ("system_u", UserKind)
          , ("object_r", RoleKind)
          ]

setInterfaceEnv :: InterfaceEnv -> T ()
setInterfaceEnv k = modify $ \s -> s{ interfaceEnv = k }

addM4Env :: Identifier -> M4Info -> T ()
addM4Env i k = do
  km <- gets m4Env
  case k of
    M4Ifdef{} -> return ()
    _         -> when (i `member` km) $
                   kcError $ DuplicateMacroDef i (findOrig i km)
  modify $ \s ->s{ m4Env = insert i k km }

lookupM4Env :: Identifier -> T (Maybe M4Info)
lookupM4Env i = do
  me <- gets m4Env
  return (lookup i me)

lookupOrigId :: Identifier -> T (Maybe Identifier)
lookupOrigId i = do
  me <- gets m4Env
  return (lookupKey i me)

findOrig :: Ord k => k -> Map k a -> k
findOrig k m = fromMaybe undefined (lookupKey k m)

lookupKey :: Ord k => k -> Map k a -> Maybe k
lookupKey k m = lookup k (mapWithKey const m)

kcError :: Error -> T ()
kcError err = do
  o <- asks kcOptions
  if not (emitErrors o)
   then return ()
   else tell [err]

kcExtendError :: Error -> T a -> T a
kcExtendError e m = do
  o <- asks kcOptions
  if not (emitErrors o)
   then m
   else do
    censor (\ds -> if null ds then []
                    else [ErrorsIn e ds]) m

definitionMap :: Policy -> Map M4Id [Either InterfaceElement SupportDef]
definitionMap p = fromListWith (++)
                  ([(elementId e,[Left e])
                   | e <- concat [es | InterfaceModule _ es <-
                                       map interface (policyModules p)]]
                   ++
                   [(i, [Right s]) | s@(SupportDef i _) <- supportDefs p])


instance KC InterfaceElement where
  kc (InterfaceElement it d i ss) = kcDefinition (it == InterfaceType) (Just d) i ss

-- | @kcDefinition@ kind check an interface/template
-- definition, ending up with a final 'InterfaceEnv' which
-- we then associate with the interface id.
kcDefinition :: KC a => Bool -> Maybe InterfaceDoc -> M4Id -> a -> T ()
kcDefinition isInterface mdoc i ss = kcExtendError (InDefinition i) $ do
  setInterfaceEnv emptyInterfaceEnv
  modify $ \s -> s{ parameterMap = Map.empty }
   -- process body of defintion...
  kc ss
   -- ..check in on what it produced
  ke  <- gets interfaceEnv
  let km = simpleKindMaps (kindMaps ke)

  pil <- checkForErrors ke km
  let ke1 = ke{parameterInfo=pil}

    -- with any errors accounted for, record resulting interface environment.
  let ke_final
       | not isInterface = ke1{kindMaps=km}
           -- clear them for interfaces only (not templates.)
       | otherwise = ke1{ kindMaps =
                           km{ iOutputMap = Map.empty
                             , outputMap  = Map.empty
			     }
			}
  addM4Env (toId i) $ M4Macro ke_final
 where
  checkForErrors ke km = do
    pm  <- gets parameterMap
    pil <- case mdoc of
      Nothing -> return (documentParameters [] pm)
      Just d -> do
        let ps = parameters d
            pil = documentParameters ps pm
        when (null (refpolicywarn ke) && any (isNothing . name) pil) $
            kcError $ UndocumentedParameters i pil
        return pil

    reportUndefinedIds km
    reportUnusedArgs   ke pil

     -- For output check: ignore identifiers that only define roles.
    let oks   = MapSet.union (iOutputMap km) (outputMap km)
    let okeys = keysSet (Map.filter (Set.singleton (posKind' RoleKind) /=) oks)
    when (isInterface && not (Set.null okeys)) $
        kcError $ IllegalSymbolDeclarations okeys
    return pil


  reportUnusedArgs ke pil
    | null (refpolicywarn ke) &&
      any ((==unusedParameterKind) . parameterKinds) pil =
      kcError $ UnusedArguments i pil
    | otherwise = return ()

  reportUndefinedIds km = do
     ir <- asks (implicitRequire.options)
     when (not ir && not (Map.null si)) $ do
          kcError $ UndefinedIds (kindMapElems si)
   where
    si = filterWithKey (const . isStatic) (inputMap km)

unusedParameterKind :: Set ParameterKind
unusedParameterKind = Set.singleton (ParameterKind Whole AnyKind)

documentParameters ::[Parameter] -> ParameterMap -> [ParameterInfo]
documentParameters ps pm =
  map doc (zip [1..max (genericLength ps) mx]
               (map Just ps++repeat Nothing))
  where
  mx = if Map.null pm then 0 else fst (findMax pm)
  doc :: (ParameterIndex, Maybe Parameter) -> ParameterInfo
  doc (i,p) =
    ParameterInfo{ name = fmap Syntax.name p
                 , optional = maybe False Syntax.optional p
                 , parameterKinds = fromMaybe unusedParameterKind (lookup i pm)
                 }

instance KC SupportDef where
  kc (SupportDef i ss) = kcDefinition False Nothing i ss

instance KC Stmt where
  kc (Tunable c s1 s2)                 = do kcCondExpr True c
                                            kcExtendError (WhenTunableTrue c)  $ kc s1
                                            kcExtendError (WhenTunableFalse c) $ kc s2
  kc (Optional s1 s2)                  = kc s1 >> kc s2
  kc (Ifdef c s1 s2)                   = kcIfdef c (kc s1) (kc s2)
  kc (Ifndef c s)                      = kcIfdef c (return ()) (kc s)
  kc (RefPolicyWarn wl)                = modify $ \s -> s{ interfaceEnv = (interfaceEnv s){ refpolicywarn = refpolicywarn (interfaceEnv s)++[w] }}
                                         where RefPolicyWarnLine w = wl
  kc (Call i al)                       = kcCall i al
  kc (Role r tas)                      = kcAddOrigin r >> kcSource tas
  kc (RoleTransition rs tas r)         = kc rs >> kcSource tas >> kc r
  kc (RoleAllow rs1 rs2)               = kc rs1 >> kc rs2
  kc (Attribute a)                     = kcAddOrigin a
  kc (Type t ts as)                    = kcAddOrigin t >> mapM_ kcAddOrigin ts >> kc as
  kc (TypeAlias t as)                  = kc t >> mapM_ kcAddOrigin as
  kc (TypeAttribute t as)              = kc t >> kc as
  kc (RangeTransition tas1 tas2 cs mr) = kc tas1 >> kc tas2 >> kc cs >> kc mr
  kc (TeNeverAllow st ps)              = kc st >> kc ps
  kc (Transition _ st t)               = kc st >> kc t
  kc (TeAvTab av st ps)                = kcAvTab av st ps -- kc st >> kc ps
  kc (CondStmt c s1 s2)                = kcCondExpr False c >> kc s1 >> kc s2
  kc (XMLDocStmt _)                    = return ()
  kc (SidStmt c)                       = kc c
  kc (FileSystemUseStmt c)             = kc c
  kc (GenFileSystemStmt c)             = kc c
  kc (PortStmt c)                      = kc c
  kc (NetInterfaceStmt c)              = kc c
  kc (NodeStmt c)                      = kc c
  kc (Define i)                        = addM4Env (toId i) (M4Ifdef (Just True))
  kc (Require r)                       = kc r
  kc (GenBoolean _ i _)                = kcAddOrigin i

kcCall :: M4Id -> [NonEmptyList (SignedId Identifier)] -> T ()
kcCall i al = do
  mm <- lookupM4Env (toId i)
  case mm of
   Just (M4Macro m) -> checkMacro m
   _ -> kcError $ UndefinedCall i
 where
  checkMacro m = do
   when argMismatch $ do
     Just oi <- lookupOrigId (toId i)
     kcError $ WrongNumberOfArguments i pil oi

   when hasWarnings $
     kcError $ RefPolicyWarnCall (toId i) warnings

   zipWithM_ kcParams (map parameterKinds pil) args_expanded
   updateAllowMap m args_expanded
   updateKindMaps m args_expanded
                  [ (kcAddIInput', inputMap)
		  , (kcAddIInput', iInputMap)
		  , (kcAddIOut',   outputMap)
		  , (kcAddIOut',   iOutputMap)
		  ]
   where
    argMismatch =
       (length args_expanded < length mpil) ||
       (length args_expanded > length pil )
    hasWarnings = not (null warnings)
    warnings    = refpolicywarn m

    pil    = parameterInfo m
    mpil   = takeWhile (not . optional) pil
    args_expanded
           = expandDollarStar (length pil) (map toList al)

updateKindMaps :: InterfaceEnv
               -> [[SignedId Identifier]]
	       -> [(Kind -> Identifier -> T a, KindMaps -> KindMap)]
	       -> T ()
updateKindMaps m al ls = do
  mapM_ (\ (f,getMap) -> do
           kms <- substKindMap al (getMap (kindMaps m))
           mapM_ (uncurry f) kms)
        ls
-- Uses of a macro means importing
updateAllowMap :: InterfaceEnv
               -> [[SignedId Identifier]]
	       -> T ()
updateAllowMap m al = do
  let am = allowMap (kindMaps m)
  kms <- substAllowMap al am
  ke  <- gets interfaceEnv
  setInterfaceEnv
    ke{ kindMaps=(kindMaps ke){
          allowMap =
	    foldl (\ amap (ai,i) -> MapSet.insert i ai amap)
	          (allowMap (kindMaps ke))
	        kms}}

{-
TODO:

 * Scope rules for required types/attributes.
    * Only allow inputMap to refer to stuff declared in
      templates/implementation of the same module,
      and only through parameters.  Whole parameters are OK.
    * Prune inputMap from illegal stuff to avoid follow-on errors.
    * Support definitions: empty inputMap except for parameters (special case of above).
    * Only allow requiring types/attributes generated in the same module.
                           inputMap  localMap  prune from inputMap at calls
       whole parameters:     yes       yes
       fragmented/same       yes       yes
       fragmented/other      no        no
       static/same           no        yes      yes
       static/other          no        no       yes
       implementation/same   yes       yes
       implementation/other  no        no
    * tunable scope rules?
    * role .. types ... ?
    * Allow types/attributes before they are declared (but warn).
 * Distinguish between domain types and domain attributes.
 * Consider doing two passes over policy:
     1. collect (i)OutputMap, compute xrefs
     2. analyze rest (input, require)
   That way, we can check xref map as we directly refer to symbols according
   to the map above.
 * Associate interfaces with templates.
 * Refine IdXrefMap into Id -> Kind -> ...
 * Treatment of identifiers declared with different kinds.
   * warn, or
   * track origin better and fix checkSameId.
 * Treatment of declarations in branches.
 * Nicer layout of parameter types for group definitions.
 * Check file contexts etc.  Define suitable scope rules.
 * Information flow inference.
 * Parse file "users", "rolemap".
 * Role declaration/use is a mess.
-}

{-
sets of signed identifiers:

examples:
  substitute { a b $1_c } for  $1 of kind  k = Whole TypeKind
                      a -> k, b -> k, $1_c -> k

             { a $1_t } for $1_s of kind k = Fragment TypeKind
                      a_s -> k, $1_t_s -> k


subst apa(p) -> apa(p)
subst apa_$1_bepa(p) -> apa_a_bepa(p)
subst $1 -> a(q) if $1 is a(q)

-}

isStatic :: IsIdentifier i => i -> Bool
isStatic = isJust . staticFragments . fragments . idString

-- | To get the effect of a macro call, we take the effects of the macro and
--   substitute for the actual parameters.
substKindMap :: [[SignedId Identifier]]
             -> KindMap
  	     -> T [(Kind, Identifier)]
substKindMap al rm = substMap al (\ k -> return $ getKind k) rm

substAllowMap :: [[SignedId Identifier]]
              -> AllowMap
	      -> T [(AllowInfo, Identifier)]
substAllowMap al rm = substMap al upd rm
  where
   upd ai = do
     t' <- substId al (avTarget ai)
       -- note: don't substitute for other fields of AllowInfo.
       -- (class and perms.)
     return $
       case t' of
        []    -> ai
	(x:_) -> ai{avTarget=x}


substMap :: [[SignedId Identifier]]
         -> (a -> T b)
         -> Map Identifier (Set a)
	 -> T [(b, Identifier)]
substMap al getI rm = do
  sss <- mapM (substId al) (keys rm)
  let kss = elems rm
  mapM (\ (k,s) -> do
          v <- getI k
	  return (v,s))
       [ (k, s) | (ss,ks) <- zip sss kss, s <- ss, k <- toList ks ]

substId :: [[SignedId Identifier]] -> Identifier -> T [Identifier]
substId al i = do
  mapM_ (checkNoM4 i) $ fst fs:map snd (snd fs)
  return $
    case wholeFragments fs of
     Just n -> fromMaybe [] (lookup n am)
     _ -> [mkId' (pos i) s | s <- substfs (fst fs) (snd fs)]
 where
  fs = fragments (idString i)

  substfs :: String -> [(ParameterIndex,String)] -> [String]
  substfs a [] = [a]
  substfs a ((n,b):r) = [a++ns++l | ns <- maybe [] (map idString) (lookup n am)
                                        , l <- substfs b r]

  am :: Map ParameterIndex [Identifier]
  am = fromList [(ParameterIndex ix,map signedId2Id (toList vs))
                 |(ix,vs) <- zip [1..] al]

  checkNoM4 :: Identifier -> String -> T ()
  checkNoM4 vi s =
     when (not (null s)) $ do
       mi <- lookupM4Env (mkId s)
       when (isJust mi) $
         kcError $ IllegalFragment s vi

expandDollarStar :: Int -> [[SignedId Identifier]] -> [[SignedId Identifier]]
expandDollarStar n [i] | simpleId i == Just dollarStar =
  [[SignedId Positive (mkId' (pos i') ("$"++show j))] | j <- [1..n]]
  where Just i' = simpleId i
expandDollarStar _ al = al

dollarStar :: Identifier
dollarStar = mkId "$*"

{- |
kcParams: check that a parameter set kan take on all the parameterkinds in kis,
  and extend the parameterKinds environment.
 * only single positive parameters can take on fragment parameterkinds.
 * complex parameters can only take on whole parameterkinds.
-}
kcParams :: Set ParameterKind -> [SignedId Identifier] -> T ()
kcParams kis a = do
  mapM_ (kcParam kis) a
  let fragmentKinds = Set.filter ((==Fragment) . fragment) kis
  when (isNothing (simpleId a) && not (Set.null fragmentKinds)) $
    kcError $ FragmentKindError a fragmentKinds

simpleId :: [SignedId Identifier] -> Maybe Identifier
simpleId [SignedId Positive i] = Just i
simpleId _                     = Nothing

-- |
kcParam :: Set ParameterKind -> SignedId Identifier -> T ()
kcParam kis = mapM_ (kcIdentifierParams kis) . m4Params . signedId2Id

kcIdentifierParams :: Set ParameterKind -> (ParameterIndex, Fragment) -> T ()
kcIdentifierParams kis (i,f) = extendParameterKinds i kis'
  where frag pk | f == Fragment = pk{ fragment = Fragment }
                | otherwise     = pk
        kis' = Set.map frag kis

extendParameterKinds :: ParameterIndex -> Set ParameterKind -> T ()
extendParameterKinds i pk =
  modify $ \s->s{ parameterMap = insertWith Set.union i pk (parameterMap s) }

kcIdentifierParam :: IsIdentifier i => Kind -> i -> T ()
kcIdentifierParam k i = mapM_ kcI (m4Params i) where
  kcI (j,f) = extendParameterKinds j (Set.singleton (ParameterKind f k))

-- | Get all the M4 parameter indices from a string, together with fragment information.
m4Params :: IsIdentifier i => i -> [(ParameterIndex, Fragment)]
m4Params i = [ (p, if isJust (wholeFragments fs) then Whole else Fragment)
             | (p,_) <- snd fs ]
  where fs = fragments (idString i)

lookupKind' :: Identifier -> T (Set PosKind)
lookupKind' i = do
  ke <- gets (kindMaps . interfaceEnv)
  me <- gets m4Env
  be <- asks base
  let f = MapSet.lookup i
  ks <- case lookup (fromId i) me of
        Just (M4IdSet ks _ ow) -> do checkRefPolicyWarnMacro i ow
                                     return (Set.map (posKind i) ks)
        _ -> return Set.empty
  return $ f be `Set.union`
           f (iOutputMap ke) `Set.union`
           f (outputMap ke)  `Set.union`
           f (localMap ke)   `Set.union`
           ks

lookupKind :: Identifier -> T (Set Kind)
lookupKind i = Set.map getKind `fmap` lookupKind' i

kcAddIOut' :: (PShow i, IsIdentifier i) => Kind -> i -> T ()
kcAddIOut' = kcAddO' (\km f -> km{ iOutputMap = f (iOutputMap km) })

kcAddOrigin :: (PShow i, IsIdentifier i, HasKind i) => i -> T ()
kcAddOrigin i = kcAddOrigin' (iKind i) i

kcAddOrigin' :: (PShow i, IsIdentifier i) => Kind -> i -> T ()
kcAddOrigin' = kcAddO' (\km f -> km{ outputMap = f (outputMap km) })

kcAddO' :: (PShow i, IsIdentifier i) =>
           (KindMaps -> (KindMap -> KindMap) -> KindMaps) ->
           Kind -> i -> T ()
kcAddO' omap k i = do
  let i' = toId i
      pk = posKind' k
  ks <- lookupKind' i'
  let defined = pk `Set.member` ks
  when (defined && k /= RoleKind) $ do
    let PosKind p _ = head (filter (==pk) (Set.elems ks))
    kcError $ DuplicateSymbolDeclaration (toId i) k (mkId' p (idString i))
  when (not defined) $ do
    ke <- gets interfaceEnv
    setInterfaceEnv ke{ kindMaps = omap (kindMaps ke) (MapSet.insert i' (posKind i' k)) }
  _ <- checkNotMacro i
  kcIdentifierParam k i

kcAddLocal :: (PShow i, IsIdentifier i, HasKind i) => i -> T ()
kcAddLocal i = kcAddLocal' (iKind i) i

kcAddLocal' :: (PShow i, IsIdentifier i) => Kind -> i -> T ()
kcAddLocal' k i = do
  ke <- gets interfaceEnv
  let i' = toId i
  setInterfaceEnv ke{ kindMaps = (kindMaps ke){ localMap = MapSet.insert i' (posKind i' k) (localMap (kindMaps ke)) } }
  _ <- checkNotMacro i
  kcIdentifierParam k i

kcAddIInput' :: (PShow i, IsIdentifier i) => Kind -> i -> T ()
kcAddIInput' = kcAddI' (\km f -> km{ iInputMap = f (iInputMap km) })

kcAddInput :: (PShow i, IsIdentifier i, HasKind i) => i -> T ()
kcAddInput i = kcAddInput' (iKind i) i

kcAddInput' :: (PShow i, IsIdentifier i) => Kind -> i -> T ()
kcAddInput' = kcAddI' (\km f -> km{ inputMap = f (inputMap km) })

kcAddI' :: (PShow i, IsIdentifier i) =>
           (KindMaps -> (KindMap -> KindMap) -> KindMaps) ->
           Kind -> i -> T ()
kcAddI' imap k i = do
  let i' = toId i
      subs = if k == TypeOrAttributeKind
             then Set.fromList [k, TypeKind, AttributeKind]
             else Set.fromList [k]
  ks <- lookupKind i'
  when (Set.null (subs `Set.intersection` ks)) $ do
    ke <- gets interfaceEnv
    setInterfaceEnv ke{ kindMaps = imap (kindMaps ke) (MapSet.insert i' (posKind i' k)) }
  kcIdentifierParam k i

instance (KC s, KC t) => KC (SourceTarget s t) where
  kc (SourceTarget st tt cs) = do
    kcSource st
    kc tt
    kc cs

kcAvTab :: AllowDeny
        -> SourceTarget (NonEmptyList (SignedId TypeOrAttributeId))
                        (NonEmptyList (SignedId Self))
        -> Permissions
	-> T ()
kcAvTab Allow (SourceTarget st tt cs) ps = do
  kcSource st
  ke <- gets interfaceEnv
    -- somewhat inscrutable, but the next couple
    -- of bindings is simply ferreting out the Identifiers
    -- that hide insde the source, target, classes and perm
  let srcs = toSList st
  let tgts = toTList tt
  let cls  = toIdList cs
  let psl  = toPList ps

   -- list of AllowInfos to augment map with; one for
   -- each target, i.e., a target is associated with
   -- a set of classes and permissions allowed over them...
  let allows = map (\ t -> allowInfo t cls psl) tgts

   -- ...which we pair with the sources in the AV decl:
  let ais  = map (\ s -> (s, allows)) srcs

    -- Having generated the associations from source
    -- to (target,perms) pairs, add them in to the
    -- AllowMap:
  let am = allowMap (kindMaps ke)
  let new_amap = foldr add am ais
       where
         -- add each (tgt,perms) pair to Set associated
	 -- with the source domain 'i'.
	add (i,ails) s = foldr (MapSet.insert i) s ails

  setInterfaceEnv ke{kindMaps = (kindMaps ke){allowMap = new_amap}}
    -- check the targets and perms, as before.
  kc tt
  kc ps

 where
  toSList s = map (toId . signedId2Id) (toList s)
  toTList s = map toSelfId (toList s)
  toIdList = map toId . toList

  toPList (Permissions ls) = map toId $ toList ls
  toPList _ = []

  toSelfId (SignedId _ Self) = mkId "self"
  toSelfId (SignedId _ (NotSelf i)) = toId i

kcAvTab _ st ps = kc st >> kc ps

kcSource :: KC a => a -> T ()
kcSource = local (\e -> e{ source = True }) . kc

kcCondExpr :: Bool -> CondExpr -> T ()
kcCondExpr t (Not c) = kcCondExpr t c
kcCondExpr t (Op c1 _ c2) = kcCondExpr t c1 >> kcCondExpr t c2
kcCondExpr t (Var b) = if t then kcAddLocal b else kcAddInput b

instance KC Require where
  kc (RequireClass c ps)   = do kcAddLocal c
                                let k = iKind (head (toList ps))
                                expandSets (Just k) ps >>= mapM_ kcAddLocal

  kc (RequireRole rs)      = mapM_ kcAddLocal rs
  kc (RequireType ts)      = mapM_ kcAddLocal ts
  kc (RequireAttribute as) = mapM_ kcAddLocal as
  kc (RequireBool bs)      = mapM_ kcAddLocal bs
  kc (RequireIfdef i t e)  = kcIfdef i (kc t) (kc e)
  kc (RequireIfndef i e)   = kcIfdef i (return ()) (kc e)

instance KC ClassId           where kc = kcAddInput
instance KC PermissionId      where kc = kcAddInput
instance KC TypeId            where kc = kcAddInput
instance KC AttributeId       where kc = kcAddInput
instance KC Sid               where kc = kcAddInput
instance KC BoolId            where kc = kcAddInput
instance KC UserId            where kc = kcAddInput
instance KC RoleId            where kc = kcAddInput
instance KC NetInterfaceId    where kc = kcAddInput
instance KC FileSystemId      where kc = kcAddInput
instance KC LevelId           where kc = kcAddInput

instance KC TypeOrAttributeId where
  kc i = do
    s <- asks source
    kcAddInput' (if s then DomainKind else TypeOrAttributeKind) i

kcIfdef :: IfdefId -> T () -> T () -> T ()
kcIfdef i t e = do
  let i' = toId i
  mm <- lookupM4Env i'
  case mm of
    Just (M4Ifdef (Just b)) -> if b then t else e
    Nothing -> do
      kcError $ UnknownIfdefId i
      mergeBranches t e
    _ -> mergeBranches t e

-- TODO: figure out more efficient handling of branches
mergeBranches :: T () -> T () -> T ()
mergeBranches t e = do
  let getBranched = do k <- gets (kindMaps . interfaceEnv)
                       m <- gets m4Env
                       return (k,m)
  (k,m) <- getBranched
  t
  bt <- getBranched
  modify $ \s -> s{ interfaceEnv = (interfaceEnv s){ kindMaps = k }
                  , m4Env = m
                  }
  e
  be <- getBranched
  mergeKindMaps bt be

type BranchedState = (KindMaps, Map Identifier M4Info)

mergeKindMaps :: BranchedState -> BranchedState -> T ()
mergeKindMaps (k1,m1) (k2,m2) = do
  checkBranchedOutputMaps (outputMap k1) (outputMap k2)
  checkBranchedOutputMaps (iOutputMap k1) (iOutputMap k2)
  let md = [ k | (k, i) <- assocs (difference m1 m2 `union` difference m2 m1)
               , case i of M4Ifdef _ -> False; _ -> True ]
  when (not (null md)) $ do
    kcError $ InconsistentMacroDefinitions md
  let mf s = MapSet.union (s k1) (s k2)
      km = KindMaps{ inputMap   = mf inputMap
                   , iInputMap  = mf iInputMap
                   , outputMap  = mf outputMap
                   , iOutputMap = mf iOutputMap
                   , localMap   = mf localMap
		   , allowMap   = mf allowMap
                   }
  modify $ \s -> s{ interfaceEnv = (interfaceEnv s){ kindMaps = km }
                  , m4Env = union m1 m2
                  }

checkBranchedOutputMaps :: KindMap -> KindMap -> T ()
checkBranchedOutputMaps m1 m2 = do
  let _ds = kindMapElems (MapSet.difference m1 m2 `MapSet.union`
                          MapSet.difference m2 m1)
  --when (not (null ds)) $ do
  --  kcError $ InconsistentSymbolDefinitions ds
  return ()

instance KC t => KC (SignedId t) where
  kc (SignedId _ t) = kc t

instance KC a => KC (NeverAllow a) where
  kc (NeverAllow ts) = kc ts
  kc (NAStarTilde s) = kc s

instance KC Permissions where
  kc (Permissions ps) = kc ps
  kc (PStarTilde st) = kc st

instance KC i => KC (StarTilde i) where
  kc Star = return ()
  kc (Tilde is) = kc is

instance KC Self where
  kc (NotSelf t) = kc t
  kc Self        = return ()

instance KC MlsRange where
  kc (MlsRange l1 l2) = kc l1 >> kc l2

instance KC a => KC (SidContext a) where
  kc (SidContext s c) = kc s >> kc c

instance KC a => KC (PortContext a) where
  kc (PortContext _ _ _ s) = kc s

instance KC a => KC (NetInterfaceContext a) where
  kc (NetInterfaceContext i s1 s2) = kcAddOrigin i >> kc s1 >> kc s2

instance KC a => KC (NodeContext a) where
  kc (NodeContext _ s) = kc s

instance KC a => KC (FileSystemUse a) where
  kc (FSUseXattr f s) = kcAddOrigin f >> kc s
  kc (FSUseTask f s)  = kcAddOrigin f >> kc s
  kc (FSUseTrans f s) = kcAddOrigin f >> kc s

instance KC a => KC (GenFileSystemContext a) where
  kc (GenFSCon _ p _ s) = kc p >> kc s

instance KC GenContext where
  kc (GenContext u r t m) = kc u >> kc r >> kc t >> kc m

instance KC FileContext where
  kc (FileContext _ _ c) = kc c
  kc (FileContextIfdef i f1 f2) = kcIfdef i (kc f1) (kc f2)
  kc (FileContextIfndef i f) = kcIfdef i (return ()) (kc f)

instance KC FilePath where
  kc (FilePath _) = return ()

instance KC FileContexts where
  kc (FileContexts fc) = kc fc

kcImplementation :: PolicyModule -> T ()
kcImplementation m = kcExtendError (InImplementation lm) $ do
 case implementation m of
  Implementation mi _ ss -> do
    when (idString mi /= baseName m) $
      kcError $ ModuleIdMismatch mi (baseName m)
{-
    mmc <- lookup (mkId (baseName m)) `fmap` asks moduleConfigMap
    mc <- case mmc of
      Just c  -> return c
      Nothing -> do kcError $ MissingModuleConfig mi
                    return Module
-}
    let mc = if required (moduleDoc (interface m)) then Base else Module
    setInterfaceEnv emptyInterfaceEnv
    modify $ \s -> s{ parameterMap = Map.empty }
    kc ss
    ke <- gets interfaceEnv
    let km = simpleKindMaps (kindMaps ke)
    ir <- asks (implicitRequire.options)
    when (not ir && mc /= Base) $ do
      let im = inputMap km
      when (not (Map.null im)) $
        kcError $ UndefinedIds (kindMapElems im)
    let fcs = fileContexts m
    kc fcs
    pm <- gets parameterMap
    when (not (Map.null pm)) $
      kcError $ IllegalParameterUse pm
    ie <- gets implEnv
    if (mi `member` ie)
      then kcError $ DuplicateIdentifier (toId mi) (toId (findOrig mi ie))
      else modify $ \s->s{ implEnv = insert mi (simpleKindMaps km) ie }

  where lm = layerModule m

checkXrefDependencies :: KindMap -> PolicyModule -> T ()
checkXrefDependencies bi pm = do
  mapM_ (checkXrefInterface bi lm) (interfaceElements (interface pm))
  checkXrefImplementation bi lm (implementation pm)
  where lm = layerModule pm

checkXrefInterface :: KindMap -> LayerModule -> InterfaceElement -> T ()
checkXrefInterface bi lm (InterfaceElement _ _ i _) =
  kcExtendError (InDefinition i) $ do
    me <- lookupM4Env (toId i)
    case me of
      Just (M4Macro ie) -> checkSameModule bi lm (kindMaps ie)
      _ -> return ()

checkXrefImplementation :: KindMap -> LayerModule -> Implementation -> T ()
checkXrefImplementation bi lm i =
  kcExtendError (InImplementation lm) $ do
    ie <- gets implEnv
    maybe (return ())
          (checkSameModule bi lm)
	  (lookup (implementationId i) ie)

checkSameModule :: KindMap -> LayerModule -> KindMaps -> T ()
checkSameModule bi lm km = do
  checkSameMap (inputMap km)
  checkSameMap (localMap km)
  where checkSameMap :: KindMap -> T ()
        checkSameMap = mapM_ checkSameId . keysSet
        checkSameId :: Identifier -> T ()
        checkSameId i
          | isJust (wholeFragments (fragments (idString i))) = return ()
          | otherwise = do
          be <- asks base
          when (Set.null (MapSet.lookup i bi) &&
                Set.null (MapSet.lookup i be)) $ do
            xr <- gets xrefs
            let lms = [ (i',lm') | (i',lm',_,_,_) <-
                        toList (MapSet.lookup
                                          (normalizeId (toId i)) xr) ]
            when (null (filter ((==lm) . snd) lms)) $ do
              let l = map snd (toList (Set.fromList [(pos j,j) | j <- map fst lms]))
              kcError $ if null l then UndefinedIdentifier i
                                  else IllegalSymbolReference i l


