{- |
Module      : $Header$
Description : SELinux policy module parsing and aggregation
Copyright   : (c) Galois, Inc.

SELinux policy module parsing and aggregation
-}

module SCD.M4.ModuleFiles(allDescendantFiles, readPolicyModule,
  readAllPolicyModules, readPolicy) where

import SCD.M4.Syntax(PolicyModule(..), LayerModule, Policy(..),
  SupportDefs, ClassPermissionDefs, GlobalBooleans, 
  ModuleConf, IfdefDecl(..))

import SCD.M4.Util ( implementationId )

import SCD.SELinux.Syntax(mkId)
import System.Directory(getDirectoryContents)

import SCD.M4.Parser(parseInterface, parseImplementation,
  parseFileContexts, parseSupportDefs, parseClassPermissionDefs,
  parseGlobalBooleans, parseModulesConf, parseIfdefDecls)

import SCD.SELinux.Parser(AccessVectors, parseClasses,
  parseAccessVectors, parseInitialSids)

import SCD.SELinux.Syntax(ClassId, Sid)

import System.FilePath(splitFileName, takeFileName, addExtension,
  dropExtension, takeExtension, extSeparator,
  dropTrailingPathSeparator, (</>), takeBaseName)

import Data.NonEmptyList(NonEmptyList)
import Data.List(sort)

import Text.Regex.Posix.Wrap((=~))
import Text.Regex.Posix.String()

allDescendantFiles :: FilePath -> IO [FilePath]
allDescendantFiles f = do
  c <- (sort . map ((f ++ "/") ++) . filter ((/=".") . take 1)) `fmap` 
       getDirectoryContents f `catch` (const (return []))
  ch <- mapM allDescendantFiles c
  return (c ++ concat ch)
  

-- | Filename extension for interface files
interfaceExtension :: String
interfaceExtension = "if"

-- | Filename extension for implementation files
implementationExtension :: String
implementationExtension = "te"

-- | Filename extension for file-context configuration files
fileContextExtension :: String
fileContextExtension = "fc"

-- | Filename extension for support-definition files
supportExtension :: String
supportExtension = "spt"

-- | Given a filename, read the interface, implementation and
-- file-configuration files (by appending ".if", ".te", and ".fc" to
-- the filename stripped from its extension).

readPolicyModule :: FilePath -> IO PolicyModule
readPolicyModule p = do
  let p' = dropExtension p
  i <- parseFile parseInterface interfaceExtension p'
  t <- parseFile parseImplementation implementationExtension p'
  let l = fst (getLayerModule p)
      lm = (l,implementationId t)
  f <- parseFile parseFileContexts fileContextExtension p'
  return PolicyModule{ layerModule = lm
                     , interface = i
                     , implementation = t
                     , fileContexts = f
                     , baseName = takeBaseName p'
                     }

-- | Extract the layer and the name from the filename prefix of a policy module
getLayerModule :: String -> LayerModule
getLayerModule p = (mkId l,mkId m)
  where (d,m) = splitFileName p
        l = takeFileName (dropTrailingPathSeparator d)

parseFile :: (FilePath -> String -> Either String a) -> String -> FilePath -> IO a
parseFile parse s p = do
  let f = addExtension p s
  c <- readFile f
  either (fail . ((f++": parser error: ")++)) 
         return 
         (parse f c)


-- | Given a directory name containing all policy modules (in subdirectories)
-- , return a list of policy 
readAllPolicyModules :: FilePath -> IO [PolicyModule]
readAllPolicyModules d = do
  ps <- filter ((==extSeparator:interfaceExtension) . takeExtension)
       `fmap` allDescendantFiles d
  mapM readPolicyModule ps

classPermissionFiles :: [FilePath]
classPermissionFiles = [ "obj_perm_sets" ]

readClassPermissionDefsFiles :: FilePath -> IO ClassPermissionDefs
readClassPermissionDefsFiles d = concat `fmap` mapM (readClassPermissionDefs . (d</>)) classPermissionFiles

supportFiles :: [FilePath]
supportFiles = [ "file_patterns"
               , "ipc_patterns"
               , "misc_patterns"
               ]

booleanFiles :: [FilePath]
booleanFiles = [ "global_booleans"
               , "global_tunables"
               ]

readSupportDefsFiles :: FilePath -> IO SupportDefs
readSupportDefsFiles d = ((specialSupportDefs++) . concat) `fmap` 
                    mapM (readSupportDefs . (d</>)) supportFiles

readGlobalBooleanFiles :: FilePath -> IO GlobalBooleans
readGlobalBooleanFiles d = concat `fmap` 
                           mapM (readGlobalBooleans . (d</>)) booleanFiles

-- Special hack to get a definition from misc_macros that ought to be in misc_patterns
specialSupportDefs :: SupportDefs
specialSupportDefs = either (error "specialSupportDefs") id 
                     (parseSupportDefs "" "define(`can_exec',`allow $1 $2:file { mmap_file_perms execute_no_trans };')")

readClassPermissionDefs :: FilePath -> IO ClassPermissionDefs
readClassPermissionDefs = parseFile parseClassPermissionDefs supportExtension

readSupportDefs :: FilePath -> IO SupportDefs
readSupportDefs = parseFile parseSupportDefs supportExtension

readGlobalBooleans :: FilePath -> IO GlobalBooleans
readGlobalBooleans = parseFile parseGlobalBooleans ""

readClasses :: FilePath -> IO ([ClassId], [ClassId])
readClasses f = do 
  kcs <- parseFile (parseFilterClasses (not.userspace)) "" f
  ucs <- parseFile (parseFilterClasses userspace) "" f
  return (kcs,ucs)
  where parseFilterClasses fi f' = parseClasses f' . unlines . filter' fi . lines
        userspace = (=~"#[ \t]*userspace")
        filter' p ll = [if p l then l else "" | l <- ll]

readAccessVectors :: FilePath -> IO AccessVectors
readAccessVectors = parseFile parseAccessVectors ""

readInitialSids :: FilePath -> IO (NonEmptyList Sid)
readInitialSids = parseFile parseInitialSids ""

_readModulesConf :: FilePath -> IO [ModuleConf]
_readModulesConf = parseFile parseModulesConf ""

readIfdefDecls :: Maybe FilePath -> IO [IfdefDecl]
readIfdefDecls mifdeffile = 
  case mifdeffile of
    Just f -> parseFile parseIfdefDecls "" f
    Nothing -> do let known s = IfdefDecl (mkId s) Nothing 
                      set s b = IfdefDecl (mkId s) (Just b)
                  return [ known "enable_mls"
                         , known "enable_mcs"
                         , known "distro_rhel4"
                         , known "distro_redhat"
                         , known "distro_debian"
                         , known "distro_gentoo"
                         , known "distro_suse"
                         , known "distro_ubuntu"
                         , known "targeted_policy"
                         , known "direct_sysadm_daemon"
                         , known "hide_broken_symptoms"
                         , set "TODO" False
                         ]

-- | Read a policy consisting of a set of modules and support
-- definitions, given path to ifdef-declaration file (unless the
-- builtin should be used), and path to reference policy directory.
readPolicy :: Maybe FilePath -> FilePath -> IO Policy
readPolicy mifdeffile pdtop = do
  let pd = pdtop </> "policy"
  ifds <- readIfdefDecls mifdeffile
  ms        <- readAllPolicyModules            (pd </> "modules")
  -- Ignore modules.conf, since it is generated from the module source.
  -- Potentially, we might want to read modules.conf and compare its content
  -- with the module source.
  -- mc        <- readModulesConf              (pd </> "modules.conf")
  let mc = []
  sds       <- readSupportDefsFiles         (pd </> "support")
  bs        <- readGlobalBooleanFiles       pd
  cpds      <- readClassPermissionDefsFiles (pd </> "support")
  (kcs,ucs) <- readClasses                  (pd </> "flask/security_classes")
  sids      <- readInitialSids              (pd </> "flask/initial_sids")
  (co,avs)  <- readAccessVectors            (pd </> "flask/access_vectors")
  return Policy{ policyModules = ms
               , supportDefs = sds
               , classPermissionDefs = cpds
               , kernelClasses = kcs
               , userClasses = ucs
               , initialSids = sids
               , commonPerms = co
               , avPerms = avs
               , globalBooleans = bs
               , modulesConf = mc
               , ifdefDecls = ifds
               }

