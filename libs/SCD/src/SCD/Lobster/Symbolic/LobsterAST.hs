{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -XScopedTypeVariables #-}
module SCD.Lobster.Symbolic.LobsterAST
    ( LobsterAST ( .. )  -- Lobster abstract syntax monoid
    , justPolicy         -- AST with just the policy part
    , showLobsterAST     -- Pretty print of LobsterAST
    , showMoniker        -- Show just the moniker of an AST
    , justStmts          -- Convert statements to LobsterAST
    , getStatements      -- Get the statements of the policy
    , getClassStatements -- Get the statements out of a top level policy
    , updateStatements   -- Update the statements of a top level policy
    , addStatements      -- Add more statements to a policy
    , replaceStatements  -- Replace the statements of a policy
    , getClassName       -- Get the class name from a policy
    , setClassName       -- Change the class name for a policy
    , getLobsterClassId  -- Get the Id of a policy
    , findFileId         -- Find the id of a file in a policy
    , findFileIds        -- Find the ids of files in a policy
    , findFileIdForce    -- Same thing but ignores possibility of failure
    , transformFileId    -- Transform a file Id to a different spec
    , mkConnection       -- Make a global connection
    ) where

import Lobster.Abs       ( Policy ( .. ), Statement ( .. ), Identifier )
import SCD.Lobster.Symbolic.AstUtil ( unPolicy, unIdentifier, mkIdentifier
                                    , isFileDecl, isDecl, getNameAndId
                                    , isClassDecl )
import Data.Monoid                  ( Monoid ( .. ) )
import Data.Maybe                   ( fromJust, catMaybes )
import Data.Map                     ( lookup, Map, fromList )
import Lobster.Print     ( printTree )
import Data.List                    ( find, delete )
import SCD.Lobster.Symbolic.Connection ( GlobalConnection ( .. ), LocalConnection ( .. ) )
import Prelude hiding ( lookup )

data LobsterAST = LobsterAST { moniker :: String
                             , policy :: Policy
                             } deriving ( Eq, Ord, Show )

justPolicy :: Policy -> LobsterAST
justPolicy p = LobsterAST { moniker = "", policy = p }

justStmts :: [ Statement ] -> LobsterAST
justStmts ss = justPolicy ( Policy ss )

-- Get the single class declaration from a Lobster policy
getClassDecl :: LobsterAST -> Statement
getClassDecl lobster =
  let ( Policy statements ) = policy lobster
      classes = filter isClassDecl statements
  in if length classes == 1
     then head classes
     else error ( "getClassDecl:" ++
                  "\nNumber of class declarations = " ++ show ( length classes ) ++
                  "\nClass declarations\n" ++ printTree classes ++
                  "\nPolicy\n" ++ printTree ( policy lobster ) ++ 
                  replicate 60 '-' ++ "\n" )

getLobsterClassId :: LobsterAST -> Identifier
getLobsterClassId lobster =
  let statements = unPolicy ( policy lobster )
      ClassDeclaration _cid1 _ids _ss = statements!!0
      DomainDeclaration id2 _cid2 _es = statements!!1
  in id2
--   in if length statements == 2
--      then let ClassDeclaration _cid1 _ids _ss = statements!!0
--               DomainDeclaration id2 _cid2 _es = statements!!1
--           in id2
--      else error ( "getClassName: " ++ show ( length statements ) ++
--                   "\n" ++ printTree ( statements ) )

getClassName :: LobsterAST -> String
getClassName = unIdentifier . getLobsterClassId

setClassName :: LobsterAST -> String -> LobsterAST
setClassName lobster name =
  let statements = unPolicy ( policy lobster )
  in if length statements == 2
     then let c@(ClassDeclaration _cid1 _ids _ss) = statements!!0
              DomainDeclaration _id2 cid2 es = statements!!1
              newDom = DomainDeclaration ( mkIdentifier name ) cid2 es
          in lobster { policy = Policy [ c, newDom ] }
     else error ( "setClassName: " ++ printTree ( statements ) )

getStatements :: LobsterAST -> [ Statement ]
getStatements l = unPolicy ( policy l )

-- A policy now has a top level class declaration that
-- contains all the statements.
getClassStatements :: LobsterAST -> [ Statement ]
getClassStatements lobster =
  let classdecl = getClassDecl lobster
      ClassDeclaration _cid _ids ss = classdecl
  in ss
--   let statements = unPolicy ( policy lobster )
--       ClassDeclaration _cid1 _ids ss = statements!!0
--       DomainDeclaration _id2 _cid2 _es = statements!!1
--   in ss

instance Monoid Policy where
    mempty = Policy mempty
    mappend ( Policy p1 ) ( Policy p2 ) = Policy ( mappend p1 p2 )

instance Monoid LobsterAST where
    mempty = LobsterAST { moniker = mempty, policy = mempty }
    mappend ( LobsterAST s1 p1 ) ( LobsterAST s2 p2 ) =
      LobsterAST { moniker = ( mappend s1 s2 )
                 , policy = ( mappend p1 p2 )
                 }

showLobsterAST :: LobsterAST -> String
showLobsterAST l = "=== " ++ moniker l ++ " ===\n" ++ printTree ( policy l )

showMoniker :: LobsterAST -> String
showMoniker l = moniker l

-- Update a top level lobster AST, consisting of a exactly two
-- statements.
updateStatements :: LobsterAST    -> -- Policy to update
                    [ Statement ] -> -- Add to internal statements at the front
                    [ Statement ] -> -- Add to internal statements at the end
                    [ Statement ] -> -- Add to external statements at the end
                    [ Statement ] -> -- Remove from internal statements
                    LobsterAST       -- Updated policy
updateStatements lobster fronts backs externals dels =
  let lob1 = addInternalFronts lobster fronts
      lob2 = addInternalBacks lob1 backs
      lob3 = addExternalStatements lob2 externals
      lob4 = deleteInternals lob3 dels
  in lob4

addStatements ::  LobsterAST -> [ Statement ] -> LobsterAST
addStatements lobster newStatements =
  let statements = getClassStatements lobster
  in lobster { policy = Policy ( statements ++ newStatements ) }

replaceStatements :: LobsterAST -> [ Statement ] -> LobsterAST
replaceStatements lobster statements = lobster { policy = Policy statements }

-- Find the id for a file domain in a policy
findFileId :: LobsterAST -> String -> Maybe Identifier
findFileId lobster fileName =
  let statements = getClassStatements lobster
      mstatement = find ( \s -> isFileDecl s fileName ) statements
  in case mstatement of
       Nothing -> Nothing
       Just ( DomainDeclaration i _c _e ) -> Just i
       Just statement -> error ( "findFileId: " ++ printTree statement )

findFileIds :: LobsterAST -> [ String ] -> [ ( String, Identifier ) ]
findFileIds lobster fileNames =
  let findings = map ( findFileId lobster ) fileNames
      noFailure = catMaybes findings
  in if length findings == length noFailure
     then zip fileNames noFailure
     else error ( "findFileIds: " ++ show fileNames ++
                  "\n" ++ printTree ( policy lobster ) )

-- And a more forceful version of the same thing (as findFileId)
findFileIdForce :: String -> LobsterAST -> Identifier
findFileIdForce fileName lobster = fromJust ( findFileId lobster fileName )

-- Transform an id of a common file from one spec to another
transformFileId :: LobsterAST -> LobsterAST -> Identifier -> Identifier
transformFileId from to ident =
  let fromIdMap = getResourceIdMap from
      mfromFileName :: Maybe String = lookup ident fromIdMap
  in case mfromFileName of
       Nothing -> error ( "transformFileId1: " ++ printTree ident )
       Just fromFileName ->
         let toNameMap = getResourceNameMap to
             mtoId = lookup fromFileName toNameMap
         in case mtoId of
              Nothing -> error ( "transformFileId2: " ++ printTree ident )
              Just toId -> toId

-- Build a map from Identifier to File name for a spec
getResourceIdMap :: LobsterAST -> Map Identifier String
getResourceIdMap lobster =
  let statements = getClassStatements lobster
      decls = filter isDecl statements
      namesAndIds = map getNameAndId decls
      reversePair :: ( a, b ) -> ( b, a )
      reversePair ( a, b ) = ( b, a )
  in fromList ( map reversePair namesAndIds )

-- Build a map from File name to Identifier for a spec
getResourceNameMap :: LobsterAST -> Map String Identifier
getResourceNameMap lobster =
  let statements = getClassStatements lobster
      decls = filter isDecl statements
      namesAndIds = map getNameAndId decls
  in fromList namesAndIds

-- Build a file connection from a file name and a from and to specification
mkConnection :: LobsterAST -> LocalConnection -> GlobalConnection
mkConnection toLobster lConn =
  let toNameMap = getResourceNameMap toLobster
      toClassId = getLobsterClassId toLobster
      mSubIdent = lookup ( localName lConn ) toNameMap
  in case mSubIdent of
       Nothing -> error ( "mkConnection: " ++ printTree ( policy toLobster ) )
       Just subIdent ->
         GlobalConnection { portConn  = localConn lConn
                          , identFrom = localIdent lConn
                          , identTo   = subIdent
                          , globName  = localName lConn
                          , fromClass = localId lConn
                          , toClass   = toClassId
                          }

----------------------------------------------------------------------
-- Some operations for adding statement, but only if they do not
-- duplicate a statement already in the policy
----------------------------------------------------------------------

-- External statements are always added at the end
addExternalStatement :: LobsterAST -> Statement -> LobsterAST
addExternalStatement lobster add =
  let Policy externals = policy lobster
  in lobster { policy = Policy ( addBackIfNotMember add externals ) }

addExternalStatements :: LobsterAST -> [ Statement ] -> LobsterAST
addExternalStatements lobster adds = foldl addExternalStatement lobster adds

-- Add an internal statement to the front of the internal statements
addInternalFront :: LobsterAST -> Statement -> LobsterAST
addInternalFront lobster add =
  let statements = unPolicy ( policy lobster )
      ClassDeclaration cid ids internals = statements!!0
      rest = tail statements
      internals' = addFrontIfNotMember add internals
  in lobster { policy = Policy ( ( ClassDeclaration cid ids internals' ) : rest ) }

addInternalFronts :: LobsterAST -> [ Statement ] -> LobsterAST
addInternalFronts lobster adds = foldl addInternalFront lobster adds

-- Add an internal statement to the front of the internal statements
addInternalBack :: LobsterAST -> Statement -> LobsterAST
addInternalBack lobster add =
  let statements = unPolicy ( policy lobster )
      ClassDeclaration cid ids internals = statements!!0
      rest = tail statements
      internals' = addBackIfNotMember add internals
  in lobster { policy = Policy ( ( ClassDeclaration cid ids internals' ) : rest ) }

addInternalBacks :: LobsterAST -> [ Statement ] -> LobsterAST
addInternalBacks lobster adds = foldl addInternalBack lobster adds

addFrontIfNotMember :: ( Eq a ) => a -> [ a ] -> [ a ]
addFrontIfNotMember a as = if a `notelem` as then a : as else as

addBackIfNotMember :: ( Eq a ) => a -> [ a ] -> [ a ]
addBackIfNotMember a as = if a `notelem` as then as ++ [ a ] else as

notelem :: ( Eq a ) => a -> [ a ] -> Bool
notelem a as = not ( elem a as )

deleteInternal :: LobsterAST -> Statement -> LobsterAST
deleteInternal lobster del =
  let statements = unPolicy ( policy lobster )
      ClassDeclaration cid ids internals = statements!!0
      rest = tail statements
      internals' = delete del internals
  in lobster { policy = Policy ( ( ClassDeclaration cid ids internals' ) : rest ) }

deleteInternals :: LobsterAST -> [ Statement ] -> LobsterAST
deleteInternals lobster dels = foldl deleteInternal lobster dels
