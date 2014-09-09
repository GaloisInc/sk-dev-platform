{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  Lobster.Gen.PermFlow
Description :  Resolving the info-flow of object permission names.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

Resolving the info-flow of object permission names.
-}
module SCD.Lobster.Gen.PermFlow where

import SCD.Lobster.Gen.CoreSyn

-- | @permToFlowDir permName@ returns the directional info
-- flow wrt. the process domain, e.g., @permToFlowDir "read"@
-- returns a left-pointing arrow, @L@. Unknown permissions map
-- to neutral.
permToFlowDir :: String -> Dir
permToFlowDir p = 
   maybe N
         id
	 (lookup p permMap)
	 

findPermFlow :: String -> Maybe Dir
findPermFlow p = lookup p permMap

permMap :: [(String, Dir)]
permMap =
  [ r "append"
  , r "create"
  , n "entrypoint"
  , n "execmod"
  , n "execute"
  , n "execute_no_trans"
  , l "getattr"
  , n "ioctl"
  , r "link"
  , r "lock"
  , r "mounton"
  , n "quotaon"
  , l "read"
  , r "relabelfrom"
  , r "relabelto"
  , r "rename"
  , r "setattr"
  , r "swapon"
  , r "unlink"
  , r "write"
     -- process object class perms
  , n "dyntransition"
  , n "execheap"
  , n "execmen"
  , n "execstack"
  , n "fork"
  , l "getattr"
  , l "getcap"
  , l "getpgid"
  , l "getsched"
  , l "getsession"
  , n "noatsecure"
  , l "ptrace"
  , n "rlimitnh"
  , r "setcap"
  , r "setexec"
  , r "setfscreate"
  , r "setpgid"
  , r "setrlimit"
  , r "setsched"
  , n "share"
  , l "siginh"
  , l "sigkill"
  , l "sigchld"
  , l "signal"
  , l "signull"
  , l "sigstop"
  , n "transition"
  ]
 where
  l p = p -=> L
  r p = p -=> R
  n p = p -=> N

  (-=>) a b = (a,b)
