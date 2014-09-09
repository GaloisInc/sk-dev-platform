module SCD.Lobster.Gen.CoreSyn.Util where

import SCD.Lobster.Gen.CoreSyn
import Data.List

combineClasses :: [Decl] -> [Decl]
combineClasses [] = []
combineClasses (d@(Class c _ _):ds) = 
  case partition (isClassNamed c) ds of
    ([],_) -> d : combineClasses ds
    (as,bs) -> (foldl joinClasses d as) : combineClasses bs
combineClasses (d:ds) = d : combineClasses ds

isClassNamed :: Name -> Decl -> Bool
isClassNamed n (Class nm _ _) = n == nm
isClassNamed _ _ = False

joinClasses :: Decl -> Decl -> Decl
joinClasses (Class nm ps1 ds1) (Class _ ps2 ds2) = 
  Class nm (nub (ps1++ps2)) (ds1++ds2)
  
