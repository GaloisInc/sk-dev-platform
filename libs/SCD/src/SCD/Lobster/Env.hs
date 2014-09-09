{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.Env
    ( Env ( .. )        -- Environment for the SIO monad
    , Mode.Mode ( .. )  -- Re-export
    , emptyEnv          -- Make an empty environment
    , isGenerate        -- Determine if mode is Generate
    , isExecute         -- Determine if mode is Execute
      -- Domain and process names
    , currentDomain     -- Get the current domain of the generation
    , currentDomainName -- Get the name of the current domain
    , setProcName       -- Set the process name
    , addDomain         -- Add a domain to the environment
      -- File names      
    , checkFileName     -- See if file already declared
    , addFileName       -- Add a new file name
    ) where

import qualified Data.Symbolic.Mode as Mode ( Mode ( .. ), isGenerate, isExecute )
import Data.Symbolic.Domain ( Domain, Hierarchy, mkDomain
                            , empty, current, domainName, descend )

data Env = Env { mode        :: Mode.Mode  -- Either Execute or Generate
               , hierarchy   :: Hierarchy  -- Currenlty not used
               , processName :: String     -- Name of the process being generated
               , fileNames   :: [ String ] -- Files in scope, to avoid regeneration
               } deriving ( Eq, Show, Ord )

isGenerate :: Env -> Bool
isGenerate = Mode.isGenerate . mode

isExecute :: Env -> Bool
isExecute = Mode.isExecute . mode

setProcName :: String -> Env -> Env
setProcName s e = e { processName = s }

-- This function will blow up if the hierarchy is empty
currentDomain :: Env -> Domain
currentDomain = current . hierarchy

-- This function will blow up if the hierarchy is empty
currentDomainName :: Env -> String
currentDomainName = domainName . current . hierarchy

-- Add domain to the hierarchy
addDomain :: String -> Env -> Env
addDomain s e = e { hierarchy = descend ( mkDomain s ) ( hierarchy e ) }

emptyEnv :: Mode.Mode -> Env
emptyEnv m = Env { mode        = m
                 , hierarchy   = empty
                 , processName = ""
                 , fileNames   = [ ]
                 }

checkFileName :: String -> Env -> Bool
checkFileName f e = f `elem` fileNames e

addFileName :: String -> Env -> Env
addFileName f e =
  if checkFileName f e
  then e { fileNames = f:fileNames e }
  else e
