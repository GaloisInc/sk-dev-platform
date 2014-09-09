{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
{- |
Module      : $Header$
Description : Tests for Shrimp's M4 subset parser
Copyright   : (c) Galois, Inc.

Tests for Shrimp's M4 subset parser
-}

module SCD.M4.Test.Parser where

import SCD.M4.Lexer(Token(..), TokenConstructor(..), scan)
import SCD.M4.Parser(parseInterface, parseImplementation, parseFileContexts)
import SCD.M4.PrettyPrint()
import System.Directory(getDirectoryContents)
import Data.List(isSuffixOf)
import Control.Monad(when)
import Test.QuickCheck.Generate (Generate(..), generateArbitrary)
import Test.QuickCheck(Arbitrary(..), CoArbitrary(..))
import System.Random(StdGen, next, split, random)
import SCD.SELinux.Test.Parser(testN, testParser, Parsable(..))
import Data.NonEmptyList(NonEmptyList)
import Prelude hiding (FilePath)
import qualified System.FilePath as FilePath

import SCD.M4.Syntax(Interface(..), ModuleDoc(..), Implementation,
  InterfaceElement, InterfaceType, InterfaceDoc(..), Require, Stmt,
  GenContext, M4Id(..), IfdefId(..), LevelId(..), ModuleId(..), MlsRange,
  XMLDoc(..), RefPolicyWarnLine(..), Version(..), FileContexts, FileContext,
  HomePath, RegexpPath(..), BoolType)

import SCD.SELinux.Syntax(mkId, IsIdentifier, Identifier, ClassId(..),
  RoleId(..), PermissionId(..), TypeId(..), AttributeId(..), TypeOrAttributeId(..),
  BoolId(..), CondExpr, SignedId, Sid(..), FileSystemId(..), NetInterfaceId(..),
  UserId(..), IPV4Address, IPV6Address, IPAddressMask, FileType,
  FilePath(..), NeverAllow, Self, Permissions, SourceTarget,
  Transition, AllowDeny, Op, Sign, StarTilde, SidContext,
  FileSystemUse, GenFileSystemContext, PortContext, Protocol,
  NetInterfaceContext, NodeContext)

instance Arbitrary Interface where
  arbitrary = generateArbitrary

instance CoArbitrary Interface where
  coarbitrary = error "Interface.coarbitrary"

instance Arbitrary Implementation where
  arbitrary = generateArbitrary

instance CoArbitrary Implementation where
  coarbitrary = error "Implementation.coarbitrary"

instance Arbitrary FileContexts where
  arbitrary = generateArbitrary

instance CoArbitrary FileContexts where
  coarbitrary = error "FileContexts.coarbitrary"

generateId :: IsIdentifier i => Int -> StdGen -> i
generateId _ r = mkId ("v"++show (fst (next r) `mod` 12))

instance Generate Identifier where
  generate = generateId
  maxLn _ _ = 2
  minLn _ _ = 2

#ifndef __HADDOCK__
deriving instance Generate AttributeId
deriving instance Generate BoolId
deriving instance Generate ClassId
deriving instance Generate FileSystemId
deriving instance Generate IfdefId
deriving instance Generate ModuleId
deriving instance Generate LevelId
deriving instance Generate M4Id
deriving instance Generate NetInterfaceId
deriving instance Generate PermissionId
deriving instance Generate RoleId
deriving instance Generate Sid
deriving instance Generate TypeId
deriving instance Generate TypeOrAttributeId
deriving instance Generate UserId
#endif

instance Generate XMLDoc where 
  generate _ _ = XMLDoc "## xml documentation here\n"
  maxLn _ _ = 2
  minLn _ _ = 2
                               
instance Generate FilePath where 
  generate _ _ = FilePath "/filepath"
  maxLn _ _ = 2
  minLn _ _ = 2
                               
instance Generate ModuleDoc where
  generate _ _ = ModuleDoc{ moduleSummary = "summary"
                          , moduleDescription = []
                          , required = True
                          , requiredDescription = "description"
                          }
  maxLn _ _ = 2
  minLn _ _ = 2

instance Generate InterfaceDoc where
  generate _ _ = InterfaceDoc{ interfaceSummary = "summary"
                             , interfaceDescription = []
                             , parameters = []
                             , infoFlow = Nothing
                             }
  maxLn _ _ = 2
  minLn _ _ = 2

instance Generate Interface
instance Generate InterfaceElement
instance Generate InterfaceType
instance Generate Require
instance Generate Stmt
instance Generate s => Generate (SidContext s)
instance Generate s => Generate (FileSystemUse s)
instance Generate s => Generate (GenFileSystemContext s)
instance Generate s => Generate (PortContext s)
instance Generate s => Generate (NetInterfaceContext s)
instance Generate s => Generate (NodeContext s)
instance Generate GenContext
instance Generate FileType
instance Generate Protocol
instance Generate IPV4Address
instance Generate IPV6Address
instance Generate IPAddressMask
instance Generate RefPolicyWarnLine where
  generate _ _ = RefPolicyWarnLine "here is a warning"
  maxLn _ _ = 2
  minLn _ _ = 2

instance Generate CondExpr
instance Generate i => Generate (SignedId i)
instance Generate MlsRange
instance (Generate s, Generate t) => Generate (SourceTarget s t)
instance Generate Permissions
instance Generate Transition
instance Generate AllowDeny
instance Generate a => Generate (NeverAllow a)
instance Generate Op
instance Generate Self
instance Generate Sign
instance Generate a => Generate (StarTilde a)
instance Generate a => Generate (NonEmptyList a)
instance Generate BoolType

instance Generate Implementation
instance Generate Version where
  generate _ _ = Version "1.0.0"
  maxLn _ _ = 2
  minLn _ _ = 2

instance Generate FileContexts
instance Generate FileContext
instance Generate HomePath
instance Generate RegexpPath where
  generate s r = if fst (random r1) then PlainPath (generate s r4)
                 else RegexpPath (if fst (random r3) then "/path/.*" else "(/path)")
                 where (r1,r2) = split r
                       (r3,r4) = split r2
  maxLn _ _ = 2
  minLn _ _ = 2

allDescendantFiles :: String -> IO [String]
allDescendantFiles f = do
  c <- (map ((f ++ "/") ++) . filter ((/=".") . take 1)) `fmap` 
       getDirectoryContents f `catch` (const (return []))
  ch <- mapM allDescendantFiles c
  return (c ++ concat ch)
  
checks :: String -> IO ()
checks directory = do
  putStrLn "\nBegin tests of the M4 interface parser"
  testN 1000 "parseInterface . render . pp" $ testParser parseInterface
  testN 1000 "parseImplementation . render . pp" $ testParser parseImplementation
  testN 1000 "parseFileContexts . render . pp" $ testParser parseFileContexts
  print directory
  c <- filter (\f -> any (`isSuffixOf` f) [".if",".te",".fc"]) `fmap`
       allDescendantFiles directory
  putStrLn $ "Scanning/parsing " ++ show (length c) ++ " files."
  mapM_ check c


instance Parsable Interface where parse = parseInterface
instance Parsable Implementation where parse = parseImplementation
instance Parsable FileContexts where parse = parseFileContexts

check :: String -> IO ()
check f = do
  c <- readFile f
  let ets = [t | t@(T _ (Error _)) <- scan f c]
  when (not (null ets)) $ fail $ f++": lexer error: " ++ show ets
  checkFile parseInterface "if" f c
  checkFile parseImplementation "te" f c
  checkFile parseFileContexts "fc" f c

checkFile :: (FilePath.FilePath -> String -> Either String a) -> 
             String -> FilePath.FilePath -> String -> IO ()
checkFile p s f c = when (s `isSuffixOf` f) $
                    either (fail . ((f++": parser error: ")++)) 
                           (const (return ())) (p f c)
