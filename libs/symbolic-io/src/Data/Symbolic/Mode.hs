{-# OPTIONS_GHC -Wall #-}
module Data.Symbolic.Mode
    ( Mode ( .. )  -- Mode of operation:
                   --    Generate: Generate a trace of the operations
                   --    Execute: Actually perform the operations
    , isGenerate   -- Is the value Generate?
    , isExecute    -- Is the value Execute?
    ) where

data Mode = Generate | Execute deriving ( Eq, Show, Ord, Enum, Bounded )

isGenerate :: Mode -> Bool
isGenerate x = x == Generate

isExecute :: Mode -> Bool
isExecute x = x == Execute
