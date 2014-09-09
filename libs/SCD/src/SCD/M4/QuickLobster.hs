--{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{- |
Module      : $Header$
Description : Quick and dirty Lobster generation of Lobster
Copyright   : (c) Galois, Inc.

Quick and dirty Lobster generation from Shrimp to inform visualization design.
-}


module SCD.M4.QuickLobster(quickLobster) where

import SCD.SELinux.Syntax(IsIdentifier(..), signedId2Id)

import SCD.M4.Syntax(Policy(..), PolicyModule(..),
  Implementation(..), Stmt(..), InterfaceElement(..),
  InterfaceType(..), interfaceElements)
import SCD.M4.KindCheck(M4Info(..), KindMaps(..), kindMaps)
import SCD.M4.Kind(emptyKindMaps)
import SCD.M4.KindInfo
import SCD.M4.Util

import qualified Lobster.Abs as L
import Lobster.Print (render, prtList)
import SCD.Lobster.Util

import Prelude hiding (lookup)

import qualified Data.Map as Map
import Data.Map(lookup, assocs, findWithDefault)
import qualified Data.NonEmptyList as NE

-----

quickLobster :: Policy -> KindInfo -> FilePath -> IO ()
quickLobster p ki _d = do
  let lms = concatMap (genModule p ki) (policyModules p)
  putStrLn (render (prtList lms))

genModule :: Policy -> KindInfo -> PolicyModule -> [L.Statement]
genModule _p ki pm = [ L.ClassDeclaration c [] is
                     , L.DomainDeclaration d (L.ClassInstantiation c [])
                     ] ++ concatMap (genInterface ki) (interfaceElements
                                                        (interface pm))
  where c = layerModule2Class lm
        d = layerModule2Identifier lm
        lm = layerModule pm
        is = genImplementation ki (implementation pm)

genImplementation :: KindInfo -> Implementation -> [L.Statement]
genImplementation ki (Implementation m _ ss) =
  [ L.PortDeclaration p L.EmptyPDT L.EmptyPDC
  | (p, _) <- assocs (portRefs ki (findWithDefault emptyKindMaps m
                                                       (implEnv ki)))
  ] ++
  genStmts ki ss

genInterface :: KindInfo -> InterfaceElement -> [L.Statement]
genInterface _  (InterfaceElement InterfaceType _ _ _) = []
genInterface ki (InterfaceElement TemplateType _ i ss) =
  [ L.ClassDeclaration c [] is ]
  where is = genStmts ki ss
        c = m4Id2Class i

genStmts :: KindInfo -> [Stmt] -> [L.Statement]
genStmts ki = concatMap (genStmt ki)

genStmt :: KindInfo -> Stmt -> [L.Statement]
genStmt ki (Tunable _ s1 s2)         = genStmts ki s1 ++ genStmts ki s2
genStmt ki (Optional s1 s2)          = genStmts ki s1 ++ genStmts ki s2
genStmt ki (Ifdef _ s1 s2)           = genStmts ki s1 ++ genStmts ki s2
genStmt ki (Ifndef _ s)              = genStmts ki s
genStmt _  (RefPolicyWarn _)         = []
genStmt ki (Call i args)             =
  case lookup (toId i) (m4Env ki) of
    Just (M4Macro ie) -> if not (Map.null (outputMap (kindMaps ie))) ||
                            not (Map.null (iOutputMap (kindMaps ie)))
                         then [L.DomainDeclaration di 
                                    (L.ClassInstantiation (m4Id2Class i) [])]
                         else []
    _                 -> []
  where di = case args of []  -> m4Id2Identifier i
                          a:_ -> fromId (signedId2Id (NE.head a))
genStmt _  (Role _ _)                = []
genStmt _  (RoleTransition _ _ _)    = []
genStmt _  (RoleAllow _ _)           = []
genStmt _  (Attribute _)             = []
genStmt _  (Type _ _ _)              = []
genStmt _  (TypeAlias _ _)           = []
genStmt _  (TypeAttribute _ _)       = []
genStmt _  (RangeTransition _ _ _ _) = []
genStmt _  (TeNeverAllow _ _)        = []
genStmt _  (Transition _ _ _)        = []
genStmt _  (TeAvTab _ _ _)           = []
genStmt ki (CondStmt _ s1 s2)        = genStmts ki s1 ++ genStmts ki s2
genStmt _  (XMLDocStmt _)            = []
genStmt _  (SidStmt _)               = []
genStmt _  (FileSystemUseStmt _)     = []
genStmt _  (GenFileSystemStmt _)     = []
genStmt _  (PortStmt _)              = []
genStmt _  (NetInterfaceStmt _)      = []
genStmt _  (NodeStmt _)              = []
genStmt _  (Define _)                = []
genStmt _  (Require _)               = []
genStmt _  (GenBoolean _ _ _)        = []

