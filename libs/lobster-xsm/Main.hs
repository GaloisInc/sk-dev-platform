{-# OPTIONS_GHC -Wall -Werror #-}
module Main(main) where

import Data.Foldable(toList)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import Data.NonEmptyList(singleton)
import qualified Data.Set as Set
import System.IO
import Text.PrettyPrint.HughesPJ(render, text, ($+$))
import Text.PrettyPrint.Pp(pp)
import qualified Text.PrettyPrint.Pp as Pp

import Lobster.Common
import Lobster.Domain(DomainId)
import Lobster.Monad(runP)
import qualified Lobster.Policy as Policy
import qualified Lobster.SELinux.SELinux as SELinux
import Lobster.SELinux.SELinux(SELinux(..))
import qualified Lobster.Syntax as Syntax
import qualified SCD.M4.Syntax as M4
import qualified SCD.SELinux.Syntax as SS

main :: IO ()
main = do
  (options,fns) <- processOptions
  domain <- parseAndInterpretPolicyFiles_ options fns
  let output = outputFile options
  domain' <- case runP (Policy.flattenDomain domain) of
    Left e -> error e
    Right v -> return v
  selinux <- case runP (SELinux.toSELinux output domain') of
    Left e -> error e
    Right v -> return v
  selinux' <- return (addTypeAttributes domain' selinux)
  selinux'' <- return (renameTypes selinux')
  selinux''' <- case insertTypeTransitions selinux'' of
    Left e -> error e
    Right v -> return v
  let selinux'''' = fixEventBinds selinux'''
  System.IO.writeFile (output ++ ".te")
    (render (pp (SELinux.typeEnforcement selinux''''))++"\n")

{-
for now, we hard code some transitions relevant to XSM event channels
todo: can these rules be generalized?

The following adds type attribute declarations for known primitive types.
-}

addTypeAttributes :: Domain -> SELinux -> SELinux
addTypeAttributes domain selinux =
  let M4.Implementation m v ss = typeEnforcement selinux in
  let ta = Policy.foldSubDomain buildTA Map.empty domain in
  let ss' = map (addTA ta) ss in
  let i = M4.Implementation m v ss' in
  selinux {typeEnforcement = i}

buildTA :: DomainId -> Domain ->
           Map.Map SS.TypeId [SS.AttributeId] ->
           Map.Map SS.TypeId [SS.AttributeId]
buildTA _ d ta =
    let n = Policy.nameDomain d in
    let (c,_) = Policy.provenanceDomain d in
    let a = case Map.lookup c dirCA of
              Just v -> v
              Nothing -> error ("unknown class name: " ++ show c) in
    Map.insert (SS.mkId (n ++ "_t")) a ta

dirCA :: Map.Map Policy.ContextClass [SS.AttributeId]
dirCA =
    Map.fromList . map mk $
       [("Domain",["domain_type"]),
        ("Event",["event_type"]),
        ("Grant",[]),
        ("MMU",[]),
        ("Shadow",[]),
        ("Xen",["xen_type"])]
  where
    mk (c,a) = (Policy.mkContextClass (Syntax.mkId c), map SS.mkId a)

addTA :: Map.Map SS.TypeId [SS.AttributeId] -> M4.Stmt -> M4.Stmt
addTA ta stmt =
    case stmt of
      M4.Type ty tys atts ->
          case Map.lookup ty ta of
            Just atts' -> M4.Type ty tys (atts ++ atts')
            Nothing -> error ("unknown type name: " ++ show ty)
      _ -> stmt

{-
The following normalizes domain type names and removes duplicate type
declarations.
-}

renameTypes :: SELinux -> SELinux
renameTypes selinux =
  let M4.Implementation m v ss = typeEnforcement selinux in
  let (tt,ss') = foldr buildTT (Map.empty,[]) ss in
  let ss'' = map (applyTT tt) ss' in
  let i = M4.Implementation m v ss'' in
  selinux {typeEnforcement = i}

buildTT :: M4.Stmt -> (Map.Map SS.TypeId SS.TypeId, [M4.Stmt]) ->
           (Map.Map SS.TypeId SS.TypeId, [M4.Stmt])
buildTT stmt (tt,stmts) =
    case stmt of
      M4.Type ty _ _ ->
          case stripInner ty of
            Nothing -> (tt, stmt : stmts)
            Just (ty',keep) ->
                (Map.insert ty ty' tt,
                 if keep then stmt : stmts else stmts)
      _ -> (tt, stmt : stmts)

first :: (a -> Maybe b) -> [a] -> Maybe b
first f l =
    case l of
      [] -> Nothing
      x : xs ->
          case f x of
            Nothing -> first f xs
            j -> j

replacePrefix :: String -> (String,String,Bool) -> Maybe (String,Bool)
replacePrefix n (p,r,k) =
    case List.stripPrefix p n of
      Just m -> Just (r ++ m, k)
      Nothing -> Nothing

stripInner :: SS.TypeId -> Maybe (SS.TypeId,Bool)
stripInner =
    let rs = map (\(s,k) -> (reverse ("_" ++ s ++ "_t"), reverse "_t", k))
               [("innerDomain",True),
                ("innerGrant",False),
                ("innerHvm",False),
                ("innerMmu",False),
                ("innerShadow",False)] in
    \ty ->
        let s = reverse (SS.idString ty) in
        case first (replacePrefix s) rs of
          Just (s',k) -> Just (SS.mkId (reverse s'), k)
          Nothing -> Nothing

applyTT :: Map.Map SS.TypeId SS.TypeId -> M4.Stmt -> M4.Stmt
applyTT tt =
    applyStmt
    where
      applyStmt stmt =
          case stmt of
            M4.Type ty tys atts ->
                M4.Type (applyTy ty) (applyTys tys) atts
            M4.TeAvTab ad (SS.SourceTarget s t cs) p ->
                let s' = applyNESidToa s in
                let t' = applyNESidSelf t in
                M4.TeAvTab ad (SS.SourceTarget s' t' cs) p
            _ -> stmt

      applyNESidToa = fmap applySidToa

      applySidToa (SS.SignedId s toa) = SS.SignedId s (applyToa toa)

      applyToa = SS.fromId . SS.toId . applyTy . SS.fromId . SS.toId

      applyNESidSelf = fmap applySidSelf

      applySidSelf (SS.SignedId s l) = SS.SignedId s (applySelf l)

      applySelf s =
          case s of
            SS.NotSelf toa -> SS.NotSelf (applyToa toa)
            SS.Self -> SS.Self

      applyTys = map applyTy

      applyTy ty =
            case Map.lookup ty tt of
              Just ty' -> ty'
              Nothing -> ty

{-
The following picks up a situation where two domains share an event channel:

{ allow $1 $3:event _;
  allow $2 $3:event bind; } $1 /= $2
=>
{ add { type_transition $1 $2:event $3; }
-}

insertTypeTransitions :: SELinux -> Either String SELinux
insertTypeTransitions s = do
  let M4.Implementation m v ss = typeEnforcement s
  ittss <- itt ss
  let ss' = ss ++ ittss
  let i = M4.Implementation m v ss'
  return s {typeEnforcement = i}

itt :: [M4.Stmt] -> Either String [M4.Stmt]
itt ss = tts where
  ec :: [(NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId),
          NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId))]
  ec = [(s, fmap (fmap unself) t) |
        M4.TeAvTab SS.Allow (SS.SourceTarget s t cs) _ <- ss,
        NE.head cs == event]

  eb :: [(NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId),
          NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId))]
  eb = [(s, fmap (fmap unself) t) |
        M4.TeAvTab SS.Allow (SS.SourceTarget s t cs) (SS.Permissions ps) <- ss,
        NE.head cs == event, NE.head ps == bind]

  tlist :: [((NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId),
              NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId)),
            [SS.SignedId SS.TypeOrAttributeId])]
  tlist = concat [[((s,s'),toList t) | (s',t') <- eb, s /= s', t'==t]
                  | (s,t) <- ec]

  tts = mapM toTT (Map.assocs (Map.fromListWith (++) tlist))

  toTT :: ((NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId),
            NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId)),
           [SS.SignedId SS.TypeOrAttributeId]) -> Either String M4.Stmt
  toTT ((s,t),nss) =
    case toList (Set.fromList nss) of
      [n] -> Right (mkTT s t n)
      ns   -> Left $ render $
                      text "\nAmbiguous type transitions:" $+$
                      Pp.above [mkTT s t n | n <- ns] $+$
                      text "This happens if you connect two domains with more than one event channel."
  mkTT :: NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId) ->
          NE.NonEmptyList (SS.SignedId SS.TypeOrAttributeId) ->
          SS.SignedId SS.TypeOrAttributeId -> M4.Stmt
  mkTT s t n = M4.Transition SS.TypeTransition
                 SS.SourceTarget{ SS.sourceTypes = s
                                , SS.targetTypes = t
                                , SS.targetClasses = singleton event
                                }
                 (SS.fromId (SS.toId (SS.signedId2Id n)))

{- allow $1 $2:event bind; =>
   allow $2 $1:event bind; -}

fixEventBinds :: SELinux -> SELinux
fixEventBinds s =
  let M4.Implementation m v ss = typeEnforcement s in
  let ss' = map feb ss in
  let i = M4.Implementation m v ss' in
  s {typeEnforcement = i}

feb :: M4.Stmt -> M4.Stmt
feb stmt =
    case stmt of
      M4.TeAvTab SS.Allow (SS.SourceTarget s t cs) (SS.Permissions ps)
        | toList cs == [event] && toList ps == [bind] ->
          M4.TeAvTab SS.Allow (SS.SourceTarget (fmap (fmap unself) t)
                                                (fmap (fmap SS.NotSelf) s)
                                                 cs) (SS.Permissions ps)
      _ -> stmt

unself :: SS.Self -> SS.TypeOrAttributeId
unself (SS.NotSelf t) = t
unself (SS.Self) = error "insertTypeTransitions: cannot handle 'self' value"

bind :: SS.PermissionId
bind = SS.mkId "bind"

event :: SS.ClassId
event = SS.mkId "event"
