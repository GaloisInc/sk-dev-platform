{-# OPTIONS_GHC -Wall -XGeneralizedNewtypeDeriving #-}
{- |
Module      :  Lobster.Gen.Monad
Description :  Managing the generation of Lobster from Reference Policy values.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

The supporting monad for doing a translation of reference policy
modules into Lobster defs.
-}
module SCD.Lobster.Gen.Monad where

import qualified SCD.M4.Syntax as M4
import SCD.M4.KindCheck ( KindInfo )
import SCD.Lobster.Gen.CoreSyn ( Decl, Module, Name )
import SCD.Lobster.Gen.Perm

import Control.Monad.State
        ( StateT(..), MonadState
	, get, put, withStateT
	)

newtype LobM a = LobM (StateT LobState IO a)
  deriving (Monad, MonadState LobState, Functor)
  
liftIO :: IO a -> LobM a
liftIO x = LobM (StateT (\ st -> x >>= \ v -> return (v,st)))

data LobState
 = LobState
     { ls_policy       :: M4.Policy
     , ls_policyModule :: M4.PolicyModule
     , ls_permMap      :: PermissionMap
     , ls_outDecls     :: [[Decl]]
     , ls_outModules   :: [Module]
     , ls_kindInfo     :: KindInfo
     }

emptyLobState :: LobState
emptyLobState = LobState
     { ls_policy       = error "emptyLobState: empty policy"
     , ls_policyModule = error "emptyLobState: empty policy module"
     , ls_permMap      = emptyPermissionMap
     , ls_outDecls     = []
     , ls_outModules   = []
     , ls_kindInfo     = error "emptyLobState: no KindInfo provided"
     }

runLobM :: LobM a -> IO a
runLobM (LobM m) = runStateT m emptyLobState >>= return.fst

getPolicy :: LobM M4.Policy
getPolicy = get >>= return.ls_policy

getPolicyModule :: LobM M4.PolicyModule
getPolicyModule = get >>= return.ls_policyModule

setPermissionMap :: PermissionMap -> LobM ()
setPermissionMap pm = do
  st <- get
  put st{ls_permMap=pm}

getPermissionMap :: LobM PermissionMap
getPermissionMap = do
  st <- get
  return (ls_permMap st)

withPolicy :: M4.Policy -> LobM a -> LobM a
withPolicy p m = do
 st <- get
 let oldp = ls_policy st
 put st{ls_policy=p}
 v <- m
 st1 <- get
 put st1{ls_policy=oldp}
 return v
 
withPolicyModule :: M4.PolicyModule -> LobM a -> LobM a
withPolicyModule p m = do
 st <- get
 let oldp = ls_policyModule st
 put st{ls_policyModule=p}
 v <- m
 st1 <- get
 put st1{ls_policyModule=oldp}
 return v

addDecl :: Decl -> LobM ()
addDecl d = do
  x <- get
  put x{ls_outDecls=[d]:(ls_outDecls x)}

addDecls :: [Decl] -> LobM ()
addDecls ds = do
  x <- get
  put x{ls_outDecls=ds : (ls_outDecls x)}
  return ()

addLobster :: Name -> [Decl] -> LobM ()
addLobster nm ds = do
  x <- get
  case break (\ v -> fst v == nm) (ls_outModules x) of
    (_,[]) -> put x{ls_outModules=(nm,ds):(ls_outModules x)}
    (as,(_,ds1):bs) -> put x{ls_outModules=(nm,ds++ds1):as++bs}

getDecls :: LobM [Decl]
getDecls = get >>= return.concat.reverse.ls_outDecls

getModules :: LobM [Module]
getModules = get >>= return.ls_outModules

declBlock :: LobM a -> LobM (a,[Decl], [Module])
declBlock m = do
  ls <- getDecls
  ms <- getModules
  x  <- get
  put x{ls_outDecls=[],ls_outModules=[]}
  v <- m
  y <- get
  put y{ls_outDecls=[ls],ls_outModules=ms}
  return (v, concat (reverse (ls_outDecls y)), ls_outModules y)

-- The type map contains definitions that expand
-- into one or more types. In the reference policy,
-- files + macros that 
getKindInfo :: LobM KindInfo
getKindInfo = get >>= return.ls_kindInfo

withKindInfo :: KindInfo -> LobM a -> LobM a
withKindInfo k (LobM x) = LobM (withStateT (\ s -> s{ls_kindInfo=k}) x)
