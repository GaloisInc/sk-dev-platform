{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.Trace
    ( Trace       -- Trace values for symbolic execution
    , Step ( .. ) -- Individual steps in the trace
    ) where

-- Traces are lists of steps
-- This is a temporary situation, we will want to replace this with other traces
-- such as Lobster abstract syntax tree definitions.
type Trace = [ Step ]
data Step = Empty
          | ReadFile FilePath
          | WriteFile FilePath String
          | PutStrLn String
  deriving ( Eq, Ord, Show )
-- GHC already knows that Trace is of class monoid
