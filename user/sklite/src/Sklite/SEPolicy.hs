{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sklite.SEPolicy
  ( -- * Generate High-level Policy from "Sklite.Layout"
    generateTe
  , generateFc
  )
where

import Data.List (intercalate)
import Data.Monoid (Monoid(..))
import Sklite.Types
import Sklite.Layout.Validation

--
-- Direct SELinux policy generation using refpolicy macros
--

newtype Policy = Policy [String] deriving (Monoid)

renderPolicy :: Policy -> String
renderPolicy (Policy xs) = unlines xs

apply :: String -> [String] -> Policy
apply fun args = Policy [fun ++ "(" ++ intercalate ", " args ++ ")"]

policy_module :: String -> String -> Policy
policy_module name version = apply "policy_module" [name, version]

typeDec :: String -> Policy
typeDec t = Policy ["type " ++ t ++ ";"]

files_type :: String -> Policy
files_type t = apply "files_type" [t]

comment :: String -> Policy
comment c = Policy ["# " ++ c]

newline :: Policy
newline = Policy [""]

allow :: String {- ^ source -} -> String {- ^ dest -} -> String {- ^ class -} -> String {- ^ vector -} -> Policy
allow s d c a = Policy [unwords ["allow", s, d ++ ":" ++ c, a ++ ";"]]

init_daemon_domain :: String {- ^ domain type -} -> String {- ^ file type -} -> Policy
init_daemon_domain p f = apply "init_daemon_domain" [p,f]

generateTe :: ExplodedLayout -> String
generateTe (ExplodedLayout (ValidatedLayout layout))
  = renderPolicy
  $ policy_module "sklite" "1.0" `mappend`
    skDirTe                      `mappend`
    (mconcat (map regionTe (sharedMemoryRegions layout))) `mappend` -- regions first, cells refer to them
    (mconcat (map cellTe   (layoutCells         layout)))

skDirType :: String
skDirType = "sk_dir_t";

skDirTe :: Policy
skDirTe    = typeDec skDirType
   `mappend` files_type skDirType

skDirFc :: [String]
skDirFc = ["/sk gen_context(system_u:object_r:"++skDirType++")"]

usesRawBinary :: Cell -> Bool
usesRawBinary c =
    case cellRunMethod c of
        RawBinary _ _ _ -> True
	_ -> False

cellTe :: Cell -> Policy
cellTe cell = mconcat $
  [comment ("Cell: " ++ cellName cell)
  ,typeDec cellDomainType
  ,typeDec cellFileType
  ,files_type cellFileType
  ,init_daemon_domain cellDomainType cellFileType
  ,allow cellDomainType skDirType "file" "write"
  ] ++
  (if usesRawBinary cell
     then [ allow cellDomainType "self" "process" "execmem" ]
     else [])
  ++
  [ allow cellDomainType ("shm_" ++ accessRegionName shmAccess ++ "_t") "file" perms
  | shmAccess <- cellSharedMemoryRegions cell
  , let perms = case accessType shmAccess of
                  MemReadOnly  -> "read_file_perms"
                  MemWriteOnly -> "rw_file_perms"
                  MemReadWrite -> "rw_file_perms"
  ] ++
  [apply i [cellDomainType]
  | i <- cellExternalInterfaces cell] ++
  [newline]
  where
  cellDomainType = "dom_" ++ cellName cell ++ "_t"
  cellFileType = cellToFileType cell

cellToFileType :: Cell -> String
cellToFileType cell = "dom_" ++ cellName cell ++ "_exec_t"

regionTe :: SharedMemoryRegion -> Policy
regionTe region = mconcat
  [comment ("Region: " ++ regionName region)
  ,typeDec typeName
  ,files_type typeName
  ,newline
  ]
  where
  typeName = regionToType region
  
regionToType :: SharedMemoryRegion -> String
regionToType region = "shm_" ++ regionName region ++ "_t"

generateFc :: ExplodedLayout -> String
generateFc (ExplodedLayout (ValidatedLayout layout)) = unlines
  $ skDirFc
 ++ [ "/sk/region_" ++ regionName region ++ ".mem gen_context(system_u:object_r:" ++ regionToType region ++ ")"
    | region <- sharedMemoryRegions layout
    ]
 ++ [ "/sk/" ++ cellProgram cell ++ " gen_context(system_u:object_r:" ++ cellToFileType cell ++ ")"
    | cell <- layoutCells layout
    ]

