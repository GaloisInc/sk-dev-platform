{-# OPTIONS_GHC -Wall -XGeneralizedNewtypeDeriving #-}
{- |
Module      :  Lobster.Gen.Util
Description :  Functions for working with Shrimp/M4 types.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

Functions for working with Shrimp/M4 types;
mostly for probing and grabbing stuff out of them.

-}
module SCD.Lobster.Gen.Util where

import qualified SCD.M4.Syntax as M4
import SCD.M4.Util
import qualified SCD.SELinux.Syntax as SE

import Control.Monad ( mplus )
import Data.NonEmptyList( NonEmptyList(..), fromList )
import Data.List
import Data.Foldable ( toList )
import Data.Char
import Data.Maybe

-- deeply buried in there...
srcTargetIdS :: SE.IsIdentifier i
             => SE.SourceTarget (NonEmptyList (SE.SignedId i)) b
	     -> String
srcTargetIdS x = SE.idString (SE.signedId2Id (hd (SE.sourceTypes x)))

srcTargetIdT :: SE.IsIdentifier i
             => SE.SourceTarget a (NonEmptyList (SE.SignedId i))
	     -> String
srcTargetIdT x = 
  case srcTargetIdTs x of
    []    -> error "srcTargetIdT: empty target list"
    (a:_) -> a

srcTargetIdTs :: SE.IsIdentifier i
              => SE.SourceTarget a (NonEmptyList (SE.SignedId i))
	      -> [String]
srcTargetIdTs x = map (SE.idString . SE.signedId2Id) (toList $ SE.targetTypes x)

srcTargetClasses :: SE.SourceTarget a b
	         -> [String]
srcTargetClasses x = map (SE.idString . SE.toId) (toList $ SE.targetClasses x)

idListString :: SE.IsIdentifier i => [NonEmptyList (SE.SignedId i)] -> [String]
idListString as = map (SE.idString . SE.signedId2Id) (concatMap toList as)

hd :: NonEmptyList a -> a
hd x = head (toList x)   

isRoleInterface :: String -> String -> Bool
isRoleInterface pre n
 | not (pre `isPrefixOf` n) = False
 | otherwise =
   case revSplitId n of
     ("role":_) -> True
     _ -> False

   
getTypeDecls :: [M4.Stmt] -> [String]
getTypeDecls ms = concatMap getTypeDecl ms
  where
    getTypeDecl m = 
     case m of
      M4.Type (SE.TypeId i) [] _ -> [SE.idString i]
      M4.TypeAlias (SE.TypeId i) ls -> 
         (SE.idString i) : map (SE.idString . SE.toId) (toList ls)
      _ -> []
      

getRoleDecls :: [M4.Stmt] -> [(String,[String])]
getRoleDecls ms = combineAssocs $ mapMaybe getRoleDecl ms
   where
    getRoleDecl m = 
     case m of
       M4.Role (SE.RoleId i) ts -> Just (SE.idString i, mapMaybe ta2I ts)
       _ -> Nothing
       

allowSource :: String 
            -> [M4.Stmt]
	    -> [(String,[String],[String])]  -- target, classes, perms
allowSource src ms = concatMap getAllow ms
 where
  getAllow m = 
    case m of
      M4.TeAvTab SE.Allow srcTarget (SE.Permissions perms)
       | srcTargetIdS srcTarget == src ->
         map (\ t -> (t,cs,ps)) (srcTargetIdTs s1)
       where
        s1 = srcTarget{SE.targetTypes=
	                fromList $
			 map selfToId $ 
			  toList   $
			   SE.targetTypes srcTarget
		      }

        cs = srcTargetClasses srcTarget
        ps = map (SE.idString . SE.toId) (toList perms)
      _ -> []

selfToId :: SE.SignedId SE.Self -> SE.SignedId SE.Identifier
selfToId (SE.SignedId s t) = SE.SignedId s t1
 where
  t1 = 
    case t of 
      SE.Self -> SE.mkId "self"
      SE.NotSelf (SE.TypeOrAttributeId i) -> i

getMacroCallsWith :: String -> [M4.Stmt] -> [String]
getMacroCallsWith wanted ms = nub $ mapMaybe getCall ms
 where
  getCall m = 
    case m of
      M4.Call (M4.M4Id f) args
       | wanted `elem` idListString args -> Just (SE.idString f)
       | otherwise -> Nothing
      _ -> Nothing



ta2I :: SE.SignedId SE.TypeOrAttributeId -> Maybe String
ta2I (SE.SignedId SE.Positive (SE.TypeOrAttributeId i)) = Just (SE.idString i)
ta2I _ = Nothing

getRequireds :: [M4.Stmt] -> [String]
getRequireds ms = nub $ concatMap getR ms
  where
   getR (M4.Require rs) = concatMap toReqTy (toList rs)
    where
     toReqTy (M4.RequireType ls) = map (SE.idString) (toList ls)
     toReqTy _ = []

   getR _ = []


-- Types used in domain contexts; i.e., in role
-- declarations, calls to macros 'known' to involve
-- domains..
getDomainTypes :: [M4.Stmt] -> [String]
getDomainTypes ms = nub $ concatMap getDomainType ms
  where
    getDomainType m = 
      case m of
        M4.Role _ ts -> mapMaybe ta2I ts
	M4.Call (M4.M4Id f) args ->
	     -- a known 'domain macro' w/ argument unmarshaller?
	   case findDomainMacro (SE.idString f) of
	     Nothing      -> []
	     Just findArg -> 
	       case findArg args of
	         Nothing -> []
		 Just a  -> [a]
        _ -> []

    findDomainMacro f = 
      case lookup f domain_macros of
        Just x -> Just x
	Nothing -> -- ToDo: consider using heuristics like *_domain()..?
	   Nothing

    domain_macros = 
      [ ("init_system_domain", pick_first_arg)
      , ("application_domain", pick_first_arg)
      , ("init_domain",        pick_first_arg)
      , ("can_exec",           pick_first_arg)
      , ("init_daemon_domain", pick_first_arg)
      ]
      
    pick_first_arg as = 
      case idListString as of
        (x:_) -> Just x
	_ -> Nothing

-- Types defined and used as a file/persistent object; i.e.,
-- with macro calls like files_type(..)
getResourceTypes :: [M4.Stmt] -> [(String,[M4.Stmt])]
getResourceTypes ms = combineAssocs $ concatMap getResType ms
  where
    getResType m = 
      case m of
	M4.Call (M4.M4Id f) args ->
	     -- a known file-like macro?
	   case findMacro (SE.idString f) of
	     Nothing      -> []
	     Just findArg -> 
	       case findArg args of
	         Nothing -> []
		 Just a  -> [(a,[m])]
        M4.TeAvTab SE.Allow srcTarget _
	 | any isPerKind (srcTargetClasses srcTarget) ->
	   map (\ t -> (t,[m])) 
	       (srcTargetIdTs s1)
         where
          s1 = srcTarget{SE.targetTypes=
	                  fromList $ map selfToId $ toList $
			   SE.targetTypes srcTarget
		        }
        _ -> []

    isPerKind f = f `elem` file_types
      where
       file_types = [ "file", "fifo_file", "dir"
                    , "blk_file", "chr_file", "lnk_file"
		    , "fifo_file", "sock_file"
		    ]

    findMacro f = 
     (lookup f known_macros) `mplus`
     case "files_" `isPrefixOf` f of
       True -> -- dicy... 
         Just (pick_arg 1)
       _ -> Nothing

    known_macros = 
      [ ("files_type",         pick_arg 0)
      , ("files_kernel_modules_filetrans", pick_arg 1)
      ]
      
    pick_arg n as = 
      case drop n (idListString as) of
        []    -> Nothing
        (x:_) -> Just x



isAllowVector :: M4.Stmt -> Bool
isAllowVector (M4.TeAvTab SE.Allow _ _) = True
isAllowVector _ = False

isAllowForSource :: String -> M4.Stmt -> Bool
isAllowForSource nm (M4.TeAvTab SE.Allow st _) 
 = nm == srcTargetIdS st
isAllowForSource _ _ = False

isAllowForTarget :: String -> M4.Stmt -> Bool
isAllowForTarget nm (M4.TeAvTab SE.Allow st _)
 = nm == srcTargetIdT st1
 where
  st1 = 
   st
    {SE.targetTypes=
	fromList $
	 map selfToId $ 
 	  toList   $ SE.targetTypes st
    }

isAllowForTarget _ _ = False

getAllowsForSource :: String -> [M4.Stmt] -> [M4.Stmt]
getAllowsForSource nm stmts = filter isAllow stmts
 where
  isAllow stmt = isAllowVector stmt && isAllowForSource nm stmt

getAllowSources :: [M4.Stmt] -> [(String, [M4.Stmt])]
getAllowSources ms = 
   combineAssocs $ 
     mapMaybe toSrc $ 
      filter isAllowVector ms
 where
  toSrc m@(M4.TeAvTab _ st _) = Just (srcTargetIdS st, [m])
  toSrc _ = Nothing

getAllowPermissions :: M4.Stmt -> [String]
getAllowPermissions (M4.TeAvTab _ _ (SE.Permissions perms))
  = map (SE.idString . SE.toId) (toList perms)
getAllowPermissions _ = []

getAllowTargets :: [M4.Stmt] -> [(String, [M4.Stmt])]
getAllowTargets ms = 
   combineAssocs $ 
     concatMap toTgt $ 
      filter isAllowVector ms
 where
  toTgt m@(M4.TeAvTab _ st _) = map (\ t -> (t,[m])) (srcTargetIdTs st1)
   where
    st1 = st{SE.targetTypes=fromList $ map selfToId $ toList $ SE.targetTypes st}
  toTgt _ = []


isLocalFileType :: String -> [M4.Stmt] -> Bool
isLocalFileType nm ms 
 | not (nm  `elem` getTypeDecls ms) = False
 | otherwise = -- a locally-declared type...
  case lookup nm (getResourceTypes ms) of
    Nothing -> False
    Just xs -> any (not.isAllowVector) xs  
                -- a bit brittle, as we depend on the knowledge
		-- that anything non-AV defines the type.

---------

lsToMaybe :: [a] -> Maybe [a]
lsToMaybe [] = Nothing
lsToMaybe xs = Just xs

combineAssocs :: (Eq a, Ord a)
              => [(a,[b])]
	      -> [(a,[b])]
combineAssocs xs = 
      mapMaybe joinGroup $ 
       groupBy (\ a b -> fst a == fst b) $
        sortBy (\ a b -> fst a `compare` fst b)
	       xs
   where
    joinGroup [] = Nothing
    joinGroup ((a,as):bs) = Just (a, as ++ concatMap snd bs)

