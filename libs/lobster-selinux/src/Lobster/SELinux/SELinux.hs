{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{- |
Module      :  $Header$
Description :  Compiling Lobster policies to information flow graphs
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Compiles the Lobster high-level policy language to an information flow graph.
-}
module Lobster.SELinux.SELinux
  ( SELinux(..)
  , compileDomain
  , toSELinux
  , prettyPrintSELinux
  , renderTypeEnforcement
  , renderFileContext
  , typeClassPermissionPort
  )

where

import qualified Data.Char as Char
import Data.NonEmptyList(singleton)
import qualified SCD.SELinux.Syntax as SS
import qualified SCD.M4.Syntax as M4
import qualified SCD.M4.PrettyPrint()
import Text.PrettyPrint.HughesPJ(Doc, render, text)
import qualified Text.PrettyPrint.Pp as Pp
import Text.Regex.Posix((=~))

import Lobster.Monad
import qualified Lobster.Abs as Abs
import Lobster.Abs (
  Connection(..),
  PortId,
  Position(..))
import qualified Lobster.Syntax as Syntax
import qualified Lobster.Domain as Domain
import Lobster.Domain(
  DomainId,
  DomainPort)
import qualified Lobster.Policy as Policy
import Lobster.Policy(
  ContextClass(..),
  Domain(..),
  Value(..))

--------------------------------------------------------------------------------
-- SELinux classes.
--------------------------------------------------------------------------------

fileContextClass :: ContextClass
fileContextClass = Policy.mkContextClass Syntax.fileClass

dirContextClass :: ContextClass
dirContextClass = Policy.mkContextClass Syntax.dirClass

typeContextClass :: ContextClass
typeContextClass = Policy.mkContextClass Syntax.typeClass

toSELinuxContextClass :: ContextClass -> P String
toSELinuxContextClass (ccl @ (ContextClass ctxt cl)) =
    if Policy.nullContext ctxt
      then return (map Char.toLower (Syntax.idString cl))
      else throwError $ "bad primitive class " ++ show ccl ++ "\n" ++
                        "primitive classes must be declared at the top level"

--------------------------------------------------------------------------------
-- Policy values.
--------------------------------------------------------------------------------

data SELinux =
    SELinux
      { typeEnforcement :: M4.Implementation
      , fileContext     :: [M4.FileContext]
      }
  deriving (Eq, Show)

toSELinux :: String -> Domain -> P SELinux
toSELinux pref obj = do
       te <- Policy.foldMSubDomain addTypeDeclaration [] obj
       te' <- Policy.foldMConnectionsDomain addTypeEnforcement te obj
       let imp = M4.Implementation
                   (SS.mkId pref)
                   (M4.Version "1.0")
                   (reverse te')
       fc <- Policy.foldMSubDomain addFileContext [] obj
       return SELinux
                {typeEnforcement = imp,
                 fileContext = fc}

    where
      domainToType o = Policy.nameDomain o ++ "_t"

      domainToClass :: Domain -> P String
      domainToClass o =
          let (ccl,_) = Policy.provenanceDomain o in
          toSELinuxContextClass ccl

      portToSubDomain :: DomainPort -> P Domain
      portToSubDomain p = case Domain.domainDomainPort p of
                            Just i  -> Policy.getSubDomain obj i
                            Nothing -> throwError "connection to System port"

      portToType p = domainToType `fmap` portToSubDomain p

      portToClass :: DomainPort -> P String
      portToClass p = domainToClass =<< portToSubDomain p

      portToPermission p = Syntax.idString (Domain.portDomainPort p)

      addTypeDeclaration :: DomainId -> Domain -> M4.Stmts -> P M4.Stmts
      addTypeDeclaration _ o tes =
          let (ccl,vs) = Policy.provenanceDomain o in
          if ccl == typeContextClass
          then case vs of
                 [StringValue t] ->
                   return (M4.Require
                             (singleton (M4.RequireType
                                           (singleton (SS.mkId t)))) : tes )
                 _ -> throwError $ "Bad arguments to "++
                                   Syntax.idString Syntax.typeClass++
                                   ": "++show vs
          else return (M4.Type (SS.mkId (domainToType o)) [] [] : tes)

      addTypeEnforcement ::
          DomainPort -> Connection -> DomainPort -> M4.Stmts -> P M4.Stmts
      addTypeEnforcement p1 _ p2 tes =
          do pt1 <- Policy.getPortTypeDomain obj p1
             pt2 <- Policy.getPortTypeDomain obj p2
             let pos1 = case Policy.positionPortType pt2 of
                          Just pos2 -> Just (Policy.invertPosition pos2)
                          Nothing -> Policy.positionPortType pt1
             case pos1 of
               Just SubjectPosition -> addTE p1 p2 tes
               Just ObjectPosition -> addTE p2 p1 tes
               Nothing -> throwError
                            ("couldn't establish subject/object " ++
                             "relationship:\n  " ++
                             Policy.prettyPrintDomainPort obj p1 ++ " : " ++
                             Policy.prettyPrintPortType pt1 ++ "\n  " ++
                             Policy.prettyPrintDomainPort obj p2 ++ " : " ++
                             Policy.prettyPrintPortType pt2)

      addTE :: DomainPort -> DomainPort -> M4.Stmts -> P M4.Stmts
      addTE ps po tes =
          do ns <- portToType ps
             no <- portToType po
             nc <- portToClass po
             od <- portToSubDomain po
             sd <- portToSubDomain ps
             let np = portToPermission po
                 (odccl,vs) = Policy.provenanceDomain od
                 o = portTypeTypeClass (odccl,vs) no
                 s = portTypeTypeClass (Policy.provenanceDomain sd) ns
             if odccl == typeTransitionContextClass
               then case vs of
{-
                      [DomainValue ti, DomainValue ni] -> do
                        t <- domainToType `fmap` getSubDomain obj ti
                        n <- domainToType `fmap` getSubDomain obj ni
-}
                      -- Beware: Worst. Hack. Ever.
                      [StringValue ti, StringValue ni] -> do
                        let p = Syntax.idString (Domain.portDomainPort po)
                            me = reverse (drop 3 (reverse no))
                        return (mkTypeTransitionRule s (me++ti) (me++ni) p :
                                mkRoleRule defaultRole (me++ni) : tes)
                      _ -> throwError $
                             "Bad arguments to "++
                             Syntax.idString typeTransitionClass++
                             ": "++show vs
               else do
               let (c,p) = case port2PermissionClass np of
                             Just cp -> cp
                             Nothing -> (nc,np)
               return (mkAllowRule s o c p : tes)

      addFileContext _ o fc =
          let (ccl,vs) = Policy.provenanceDomain o in
          if ccl == fileContextClass || ccl == dirContextClass then
            case vs of
              [StringValue s] ->
                return $ M4.FileContext (M4.Path (M4.RegexpPath s)) Nothing
                           (Just (M4.GenContext fc_user fc_role
                                     (SS.mkId (domainToType o))
                                     fc_mlsRange)) : fc
              _ -> throwError "bad File arguments"
          else return fc


typeTransitionContextClass :: Policy.ContextClass
typeTransitionContextClass = Policy.mkContextClass typeTransitionClass

typeTransitionClass :: Abs.ClassId
typeTransitionClass = Syntax.mkId "TypeTransition"

portTypeTypeClass :: (ContextClass, [Value]) -> String -> String
portTypeTypeClass (ccl,[StringValue s]) _ | ccl == typeContextClass = s
portTypeTypeClass _                    no = no

mkAllowRule :: String -> String -> String -> String -> M4.Stmt
mkAllowRule ns no nc np =
  M4.TeAvTab SS.Allow
    (SS.SourceTarget
      { SS.sourceTypes = singleton (SS.SignedId SS.Positive (SS.mkId ns))
      , SS.targetTypes = singleton (SS.SignedId SS.Positive (SS.NotSelf
                                                               (SS.mkId no)))
      , SS.targetClasses = singleton (SS.mkId nc)})
    (SS.Permissions (singleton (SS.mkId np)))

mkTypeTransitionRule :: String -> String -> String -> String -> M4.Stmt
mkTypeTransitionRule ns t n p =
  M4.Transition SS.TypeTransition
    (SS.SourceTarget
      { SS.sourceTypes = singleton (SS.SignedId SS.Positive (SS.mkId ns))
      , SS.targetTypes = singleton (SS.SignedId SS.Positive (SS.mkId t))
      , SS.targetClasses = singleton (SS.mkId p) })
    (SS.mkId n)

mkRoleRule :: SS.RoleId -> String -> M4.Stmt
mkRoleRule r t = M4.Role r [SS.SignedId SS.Positive (SS.mkId t)]

-- Default values to use for file contexts (hard-wired for now)

fc_user :: SS.UserId
fc_user = SS.mkId "system_u"

fc_role :: SS.RoleId
fc_role = SS.mkId "object_r"

fc_mlsRange :: M4.MlsRange
fc_mlsRange = M4.MlsRange s0 s0 where s0 = SS.mkId "s0"

-- Default role for role transition

defaultRole :: SS.RoleId
defaultRole = SS.mkId "unconfined_r"

prettyPrintSELinux :: SELinux -> String
prettyPrintSELinux selinux = render $ Pp.above $
  [ text "--- Type enforcement (.te) file ---"
  , ppTypeEnforcement selinux
  , text "--- File context (.fc) file ---"
  ]
  ++
  ppFileContext selinux


ppTypeEnforcement :: SELinux -> Doc
ppTypeEnforcement = Pp.pp  . typeEnforcement

ppFileContext :: SELinux -> [Doc]
ppFileContext = map Pp.pp . fileContext

renderTypeEnforcement :: SELinux -> String
renderTypeEnforcement selinux = (render . ppTypeEnforcement) selinux
                             ++ "\n" -- Files in POSIX end in newline

renderFileContext :: SELinux -> String
renderFileContext selinux = (render . Pp.above . ppFileContext) selinux
                             ++ "\n" -- Files in POSIX end in newline

-- | Construct magic Type ports from  class-permission map
typeClassPermissionPort :: SS.ClassId -> SS.PermissionId -> PortId
typeClassPermissionPort c p = Syntax.mkId (SS.idString c ++
                                           typeClassPermissionSeparator ++
                                           SS.idString p)

-- | Deconstruct magic Type port into class and permission
port2PermissionClass :: String -> Maybe (String, String)
port2PermissionClass s | m == ""   = Nothing
                       | otherwise = Just (b,a)
  where (b,m,a) = s =~ typeClassPermissionSeparator

typeClassPermissionSeparator :: String
typeClassPermissionSeparator = "__"

flattenDomain :: Domain -> Domain
flattenDomain domain =
    case runP (Policy.flattenDomain domain) of
      Left err ->
          error ("ERROR: couldn't flatten the Lobster policy file:\n" ++ err)
      Right domain' -> domain'

compileDomain :: String -> Domain -> SELinux
compileDomain output domain =
    let domain' = flattenDomain domain in
    domainToSELinux output domain'

domainToSELinux :: String -> Domain -> SELinux
domainToSELinux moduleName domain =
    case runP (toSELinux moduleName domain) of
      Left err ->
          error ("ERROR: couldn't generate native SELinux from the " ++
                 "Lobster policy file:\n" ++ err)
      Right selinux -> selinux
