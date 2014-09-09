{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

{- |
Module      : $Header$
Description : Validation of M4 type statements
Copyright   : (c) Galois, Inc.

Validation of M4 type statements
-}

module SCD.M4.CheckTypeStmt(checkInterface) where

import SCD.M4.Syntax(Interface(..), InterfaceElement(..),
  InterfaceType(..), Stmt(..), M4Id)

-- | Return a list of interface names paired with type-declaration
-- statements, found in interface bodies that declare new types.

checkInterface :: Interface -> [(M4Id, Stmt)]
checkInterface (InterfaceModule _ ies) = concatMap checkInterfaceElement ies

checkInterfaceElement :: InterfaceElement -> [(M4Id, Stmt)]
checkInterfaceElement (InterfaceElement InterfaceType _ i ss) = 
    [(i,e) | e <- checkStmts ss]
checkInterfaceElement (InterfaceElement TemplateType _ _ _) = []

checkStmts :: [Stmt] -> [Stmt]
checkStmts = concatMap checkStmt

checkStmt :: Stmt -> [Stmt]
checkStmt (Tunable _ s1 s2)         = checkStmts s1 ++ checkStmts s2
checkStmt (Optional s1 s2)          = checkStmts s1 ++ checkStmts s2
checkStmt (Ifdef _ s1 s2)           = checkStmts s1 ++ checkStmts s2
checkStmt (Ifndef _ s1)             = checkStmts s1
checkStmt (RefPolicyWarn _)         = []
checkStmt (Call _ _)                = []
checkStmt (Role _ _)                = []
checkStmt (RoleTransition _ _ _)    = []
checkStmt (RoleAllow _ _)           = []
checkStmt (Attribute _)             = []
checkStmt s@(Type _ _ _)            = [s]
checkStmt (TypeAlias _ _)           = []
checkStmt (TypeAttribute _ _)       = []
checkStmt (RangeTransition _ _ _ _) = []
checkStmt (TeNeverAllow _ _)        = []
checkStmt (Transition _ _ _)        = []
checkStmt (TeAvTab _ _ _)           = []
checkStmt (CondStmt _ s1 s2)        = checkStmts s1 ++ checkStmts s2
checkStmt (XMLDocStmt _)            = []
checkStmt (SidStmt _)               = []
checkStmt (FileSystemUseStmt _)     = []
checkStmt (GenFileSystemStmt _)     = []
checkStmt (PortStmt _)              = []
checkStmt (NetInterfaceStmt _)      = []
checkStmt (NodeStmt _)              = []
checkStmt (Define _)                = []
checkStmt (Require _)               = []
checkStmt (GenBoolean _ _ _)        = []
