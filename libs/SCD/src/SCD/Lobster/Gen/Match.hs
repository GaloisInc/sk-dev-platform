{-# OPTIONS_GHC -Wall -XGeneralizedNewtypeDeriving #-}
{- |
Module      :  Lobster.Gen.Match
Description :  Managing the generation of Lobster from Reference Policy values.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

The supporting monad for doing a translation of reference policy
modules into Lobster defs.
-}
module SCD.Lobster.Gen.Match where

import SCD.Lobster.Gen.CoreSyn
import SCD.Lobster.Gen.Perm
import SCD.Lobster.Gen.PermFlow

import SCD.Lobster.Gen.Monad
import SCD.Lobster.Utilities ( capitalize )
import SCD.Lobster.Gen.Util
import qualified SCD.M4.Syntax as M4
import qualified SCD.M4.Util as M4
import qualified SCD.SELinux.Syntax as SE

import SCD.M4.KindInfo ( portRefs, KindInfo(..) )
import SCD.M4.Kind     ( KindMaps(..), AllowInfo(..) )
import SCD.M4.Util

import Data.Foldable ( toList )
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as M

import System.FilePath ( takeBaseName )
import Text.Happy.ParserMonad(Pos(..))

--import Debug.Trace

genLobsterPolicyModule :: M4.PolicyModule
		       -> LobM ()
genLobsterPolicyModule pm = do
  let (M4.Implementation (M4.ModuleId m) _ body) = M4.implementation pm
  let mStr = SE.idString m
  withPolicyModule pm $ do
      genClass True mStr body
      mapM_ (genLobsterModule mStr)
	    (M4.interfaceElements $ M4.interface pm)

genLobsterModule :: String
                 -> M4.InterfaceElement
		 -> LobM ()
genLobsterModule clsName ifa = do
{-
  pm <- getPolicyModule
  let layer   = case M4.layerModule pm of { (M4.LayerId i ,_) -> SE.idString i}
  let bName   = M4.baseName pm
  let lobOut  = layer ++ '/':bName
  liftIO (putStrLn ("Processing: " ++ lobOut))
-}
  mb <- matchInterfaceKind ifa
  maybe (return ()) (\ f -> f clsName ifa) mb

matchInterfaceKind :: M4.InterfaceElement -> LobM (Maybe GenMethod)
matchInterfaceKind ifa =
  case rev_nm of
    []    -> return Nothing
    (x:_) ->
      case lookup x kindMap of
        Just f -> return (Just f)
	Nothing -> return (Just genRole)
	  -- ToDo: determine kind by looking at shape etc.
	  --         - if it contains one or more allow vectors (or macro
	  --           calls to such), label as being of certain kind.
          --         - if
          --
	  -- Q: build up classification guidance by processing supporting
	  --    macro definitions (e.g., patterns) first, classifying them
	  --    as being of a part

 where
  rev_nm   = revSplitId iface_nm
  iface_nm = SE.idString (M4.elementId ifa)

  kindMap :: [(String, GenMethod)]
  kindMap =
    [ ( "role",     genRole)
    , ( "domtrans", genDomainTransfer)
    ]

type GenMethod = String -> M4.InterfaceElement -> LobM ()

genDomainTransfer :: GenMethod
genDomainTransfer _nm _ifa = return ()

genRole :: GenMethod
genRole n ifa = genClass False n iface_stmts
 where
--  inm_rpol = SE.idString (M4.elementId ifa)
  (M4.InterfaceElement _ _ _ iface_stmts)  = ifa

genClass :: Bool -> String -> M4.Stmts -> LobM ()
genClass isImpl inm_rpol iface_stmts = do
   ki  <- getKindInfo
   pm  <- getPolicyModule
   mp  <- getPermissionMap
   let impBody = case M4.implementation pm of { M4.Implementation _ _ b -> b }
   let layer   = case M4.layerModule pm of { (M4.LayerId i ,_) -> SE.idString i}
   let bName   = M4.baseName pm
   let lobOut  = layer ++ '/':bName
   let (cls,topD) = mkCls mp ki impBody
   addLobster (Name (lobOut ++ ".lst")) $
       (if isImpl
         then \ xs ->
	   ( newComment ("Ref policy role: " ++ inm_rpol)
                -- ToDo: support translation straight from the tresys.com repo?
           : refPolLink (lobOut ++ ".if") : xs)
	 else id) [cls]
   () <- addDecls topD
   return ()
  where
    inm_splt = splitId inm_rpol
    inm      = unsplitId inm_splt

    mkCls mp ki bs = (newClass clsName params (stmts ++ domCs ++ domPorts), topD)
     where
      clsName = Name (capitalize inm)
      params  = []

      doms    = getDomainTypes bs
      domCs
       | isImpl    = concatMap (genDomainClasses mp bs) doms
       | otherwise = []

      stmts   = concat [ ports, types, internals, connections ]
      ports   = concatMap toPortDecl allows
      types
       | not isImpl = []
       | otherwise  =
          newComment "" :
           concatMap (toTypeDecl inm (map (dropIdSuffix "t") doms)) bs

      domPorts
       | not isImpl = []
       | otherwise  =
          map ((withConnection me R) . newPort) $ nub $
	    concatMap (\ (d,pts) ->
	                 map (\p -> newNm (normalizeTypeName inm_rpol d) p) pts)
	              ps
         where
	    -- ToDo: disambiguate port names by their (local) domain types
	    --
          ps = concatMap (findPerms False) doms
	  newNm "" p = Name p
	  newNm d  p = Name (d ++ '_':'_':p)

      mbEnv = M.lookup (SE.mkId inm_rpol) (implEnv ki)

      topD
       | not isImpl = []
       | otherwise  =
         (newDomain (Name inm) clsName []) :
         (mapMaybe (\ (x,p) ->
	            case x of
		      "" -> Nothing
		      _  -> Just (neutral (domPort (Name inm) (Name "active"))
                                          (domPort (Name x) (Name p))))
	      (map (\ x -> (x,"active")) xs ++
	       nub (
	       concatMap (\ (i,ps) -> map (\ p -> (i,p)) ps)
	                 (concatMap (findPerms True) doms)))
			   -- pair all targets with their permissions
			 )


          -- find all permissions for a given local domain..
	  -- for fear of trivial name mismatches stopping the
	  -- info flowing through, we take them all rather than
	  -- try to filter them to a specific local domain type.
	  -- As we're nub'ing the result list for all domain types
	  -- embodied by local.
      findPerms withTgt i =
	  case mbEnv of
	    Nothing
	      | withTgt   -> []
	      | otherwise -> [(i,[])]
	    Just km ->
	      case M.lookup (SE.mkId i) (allowMap km) of
	        Just sal
		 | not withTgt ->
		    [(i, map (\ (t, p) -> idStringPrefix inm_rpol (SE.idString t) p) $
		              concatMap (\ av -> map (\ p -> (avTarget av, p))
			                             (avPerms av))
					(toList sal))]
		 | otherwise ->
		   map (\ avs ->
		           let (is, pre) = idPosString (avTarget avs) in
		           ( normalizeTypeName i is
		           , map (\ idn -> pre ++ SE.idString idn) (avPerms avs)
			   ))
		       (toList sal)
		_
		 | withTgt   -> []
		 | otherwise -> [(i,[])]


      xs =
	 mapMaybe
	    (\ (x,_) -> ofInterest x) $
	  maybe []
	        (M.toList  . portRefs ki)
		mbEnv

      ofInterest x =
	  case revSplitId (SE.idString x) of
	      -- weed out self references.
	    (v:_) | v /= inm_rpol -> Just v
	    _ -> Nothing

      internals
       | not isImpl = []
       | otherwise  =
        [ newComment ""
	, newDomain  (Name "me") (Name "Process") []
        ]

      me = domPort (Name "me") (Name "active")

      addDomainTransition =
        let
	 e_port =
	   withConnection me R $
	    withConstraint (PortType (tname "exec")) $
	     newPort (Name "executable")
	in
        case filter isDomainTransition iface_stmts of
	  (M4.Transition _ _src_tgt (SE.TypeId _):_) ->
	    (e_port:)
	  ((M4.Call _ args) : _) ->
	    case concatMap toList args of
	      [_,_,_] -> (e_port:)
              _ -> id
	  _ -> id
       where
        isDomainTransition (M4.Transition SE.TypeTransition _stgt _t) = True
	isDomainTransition (M4.Call (M4.M4Id i) _)
	  = SE.idString i == "domtrans_pattern"
	isDomainTransition _ = False -- trace (show x) $ False

        -- wire up the internals..
      connections
       | not isImpl = []
       | otherwise  = addDomainTransition $
        [ withConnection me{portLabel=Name "transition"} N
                         (newPort (Name "create"))
        ]

     -- ad-hoc: locate access vectors of the form:  allow $2 ... perm
    allows = getAllowsForSource "$2" iface_stmts

genDomainClasses :: PermissionMap -> [M4.Stmt] -> String -> [Decl]
genDomainClasses mp ms d0 =
   concatMap mkLocalType local_fs_for_d ++
   concatMap allowPort allows
{-
  [ newComment ("Ref policy domain: " ++ d)
  , newClass d []
             (concat
	       [ [newDomain "me" "Process" [] ]
	       , concatMap mkLocalType local_fs_for_d
	       , concatMap allowPort allows
	       ])
  ]
-}
 where
  d      = dropIdSuffix "t" d0
  me     = domPort (Name "me") (Name "active")

  allows = allowSource d0 ms
  --UNUSED:macros = getMacroCallsWith d0 ms

  fs     = getResourceTypes ms

  avs_for_d = getAllowsForSource d0 ms
   -- the file AVs with 'd0' as a source.
  fs_for_d  = getResourceTypes avs_for_d

   -- if 'd0' is the sole domain with AV decls for a file type,
   -- then we may push it inside the domain class.
   --
  local_fs_for_d =
   map (\ (f,ds) -> (f, filter isAllowVector ds)) $
    filter (\ (f,_) ->
               case lookup f fs of
	         Nothing -> False
		 Just xs -> all (\ x -> d0==fst x) (getAllowSources xs))
           fs_for_d

  mkLocalType (f,avs) =
    ( newDomain pretty_f (Name "File") []
    : map (\ p -> connect (permToFlowDir p) me (domPort pretty_f (Name p)))
          (expandPerm $ concatMap getAllowPermissions avs)
    )
   where
    expandPerm ps = nub (concatMap (expandPermission mp) ps)

    pretty_f = Name (d ++ '_':dropIdSuffix "t" f)

  allowPort ("self",_,_)  = []
  allowPort (_tgt,cls,ps) =
    concatMap (\ c -> map (newP c) ps) cls
   where
    newP c p =
      withConnection me R $
	withConstraint (PortType (tname c)) $
         newPort (Name (d++'_':p))

-- from a (target) Identifier, determine its domain and
-- port prefix to use. If module name matches domain name,
-- assume no prefix.
idPosString :: SE.Identifier -> (String,String)
idPosString i
 | b /= is   = (b,is++"_")
 | otherwise = (is,"")
 where
  is = normalizeTypeName "" (SE.idString i)
  b  = System.FilePath.takeBaseName fp
  (Pos fp _ _ _) = SE.pos i

idStringPrefix :: String -> String -> SE.Identifier -> String
idStringPrefix inm d i
 | inm /= ds && take 1 ds /= "$" = ds ++ '_':is
 | otherwise = is
 where
  ds = normalizeTypeName "" d
  is = normalizeTypeName "" (SE.idString i)

normalizeTypeName :: String -> String -> String
normalizeTypeName inm t =
  case reverse (case revSplitId t of { ("t":ts) -> ts ; ts -> ts }) of
   (a:as)
    | a `isPrefixOf` inm -> unsplitId as
   as -> unsplitId as

toTypeDecl :: String -> [String] -> M4.Stmt -> [Decl]
toTypeDecl im blackList s = mapMaybe toNewT (getIds s)
 where
  toNewT (i,is)
   | any (\ x -> x `isPrefixOf` SE.idString i) blackList = Nothing
   | otherwise =
     case map (\ (SE.TypeId v) -> (normalizeTypeName im (SE.idString v))) (i:is) of
      (a:as) -> Just (newT a (map Name as))
      _ -> Nothing

  getIds x =
   case x of
    M4.Type i is _ -> [(i,toList is)]
    M4.TypeAlias i is -> map (\ t -> (t,[])) (i:toList is)
    _ -> []

  newT "" _  = newDomain (Name hd_im) (Name "Process") []
               where hd_im   = head (splitId im)
  newT x  xs = newType (tname x) xs

toPortDecl :: M4.Stmt -> [Decl]
toPortDecl (M4.TeAvTab _ _ perms) =
  map (\ pid -> withConstraint (PortType (tname "command")) $
                dirPort (Name (SE.idString pid)) R)
      (toList ps)
 where
  (SE.Permissions ps) = perms
toPortDecl _ = []

tname :: String -> Name
tname s = Name ("T"++s)

refPolLink :: String -> Decl
refPolLink l =
 newComment ("URL: http://oss.tresys.com/repos/refpolicy/trunk/policy/modules/" ++ l)

