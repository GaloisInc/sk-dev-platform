{- |
Module      : $Header$
Description : Global options for reference policy processing
Copyright   : (c) Galois, Inc.

Global options for reference policy processing
-}

module SCD.M4.Options(Options(..), defaultOptions) where

-- | Global options for reference policy processing
data Options = Options{ implicitRequire :: Bool
                      , group           :: Bool
                      , xmlErrorOutput  :: Bool
                      , ifdefDeclFile   :: Maybe FilePath
                      }
  deriving Show

-- | Default options for reference policy processing
defaultOptions :: Options
defaultOptions = Options{ implicitRequire = False
                        , group           = True 
                        , xmlErrorOutput  = False
                        , ifdefDeclFile   = Nothing
                        }

