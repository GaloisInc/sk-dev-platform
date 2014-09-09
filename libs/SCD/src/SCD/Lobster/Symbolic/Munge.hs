{-# OPTIONS_GHC -Wall -Werror -XScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- Munge a tree of LobsterASTs, this means to find references to files at a
-- higher level in the tree that are overlapping files used at a lower
-- level in the tree, and provide a path for the higher level domain to
-- use the file in the lower level domain, but adding a connection from
-- the "p.active" port in the higher level domain to the in port of the
-- lower level domain
--------------------------------------------------------------------------------
module SCD.Lobster.Symbolic.Munge
    ( MungeTree             -- Tree of classes
    , munge                 -- Munge the tree
    , whatFiles             -- What files are in a policy
    , whatProcesses         -- What processes are in a policy
    , whatFileDecls         -- Temporary
    ) where

import Lobster.Abs ( Statement ( .. ), Identifier
                              , Connection ( .. ) )
import SCD.Lobster.Symbolic.AstUtil     -- *** FIX ME *** ---
import SCD.Lobster.Symbolic.Tree ( Tree ( .. ) )
import Data.Maybe ( catMaybes )
import Data.List ( intersect, nub, partition, concat )
import SCD.Lobster.Symbolic.Connection  
   ( PreConnection ( .. ), mkPre, GlobalConnection ( .. ) )
import SCD.Lobster.Symbolic.LobsterAST
   ( LobsterAST ( .. ), getClassStatements, mkConnection, getLobsterClassId
   , updateStatements, getClassName )
-- import Debug.Trace                      ( trace )
-- import Lobster.Print         ( printTree )

type MungeTree = Tree LobsterAST

-- What are the processes of a domain?
whatProcesses :: LobsterAST -> [ PreConnection ]
whatProcesses lobster =
  let nameAndIds = map getProcNameAndId
                       ( filter isProcDomain ( getClassStatements lobster ) )
  in map mkPre nameAndIds

-- What are the files of a domain?
whatFiles :: LobsterAST -> [ PreConnection ]
whatFiles lobster = let fileNamesAndIds = whatFilesAndIds lobster
                    in map mkPre fileNamesAndIds

whatFilesAndIds :: LobsterAST -> [ ( String, Identifier ) ]
whatFilesAndIds lobster =
  map getNameAndId ( filter isFileDomain ( getClassStatements lobster ) )

whatFileDecls :: LobsterAST -> [ Statement ]
whatFileDecls lobster = filter isFileDomain ( getClassStatements lobster )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Munge two top level Lobster ASTs, each have a single class declaration
munge :: LobsterAST -> LobsterAST -> ( LobsterAST, LobsterAST )
munge super sub =
  let superFiles :: [ PreConnection ]
      superFiles = nub ( whatFiles super )
      subFiles :: [ PreConnection ]
      subFiles = nub ( whatFiles sub )
      commonFiles :: [ PreConnection ]
      commonFiles = nub ( intersect superFiles subFiles )
      superProcs :: [ PreConnection ]
      superProcs = whatProcesses super
      subProcs :: [ PreConnection ]
      subProcs = whatProcesses sub
      commonProcs :: [ PreConnection ]
      commonProcs = nub ( intersect superProcs subProcs )
      ( super1, sub1 ) = mungeProcesses commonProcs super sub
      ( super2, sub2 ) = mungeFiles commonFiles super1 sub1
  in ( super2, sub2 )

mungeFiles :: [ PreConnection ] -> -- Files to munge
              LobsterAST -> -- Super domain spec
              LobsterAST -> -- Sub domain spec
              ( LobsterAST, LobsterAST ) -- New super and sub domain spec
mungeFiles preConns super sub =
  let superStatements = getClassStatements super
      superId = getLobsterClassId super
      sharedFileNames = map preName preConns
      sharedFileIds = map preIdent preConns
      ( nukes, reduced ) = partition ( isFileDecls sharedFileNames ) superStatements
      moreNukes = filter ( \s -> connectionForIds s sharedFileIds ) reduced
      localToConns1 = map ( \s -> connectionTos preConns superId s ) reduced
      localToConns2 = catMaybes localToConns1
      localFromConns1 = map ( \s -> connectionFroms preConns superId s ) reduced
      localFromConns2 = catMaybes localFromConns1
      -- Convert local to global connections
      globalToConns :: [ GlobalConnection ] = map ( mkConnection sub ) localToConns2
      globalFromConns :: [ GlobalConnection ] = map ( mkConnection sub ) localFromConns2
      toIntExts = map ( transformToInternal ( Just NeutralConnection ) "ext" "in" Nothing ) globalToConns
      fromIntExts = map ( transformFromInternal ( Just NeutralConnection ) "ext" "out" ) globalFromConns
      toExts = concat ( map fstOfThree toIntExts )
      fromExts = concat ( map fstOfThree fromIntExts )
      toInts = concat ( map sndOfThree toIntExts )
      fromInts = concat ( map sndOfThree fromIntExts )
      externals = concat ( map thdOfThree toIntExts ) ++
                  concat ( map thdOfThree fromIntExts )
      inPort = mkPortDeclaration "in"
      outPort = mkPortDeclaration "out"
      extPort = mkPortDeclaration "ext"
      newPorts = [ inPort, outPort ]
  in -- trace ( "mungeFiles: moreNukes = " ++ printTree moreNukes ++
     --         "\nreduced2 = " ++ printTree reduced2 )
           ( updateStatements super [ extPort ] ( toExts ++ fromExts ) [ ]
                                  ( nukes ++ moreNukes )
           , updateStatements sub newPorts ( toInts ++ fromInts ) externals [ ] )

fstOfThree :: ( a, b, c ) -> a
fstOfThree ( a, _b, _c ) = a
sndOfThree :: ( a, b, c ) -> b
sndOfThree ( _a, b, _c ) = b
thdOfThree :: ( a, b, c ) -> c
thdOfThree ( _a, _b, c ) = c

mungeProcesses :: [ PreConnection ] -> -- Processes to munge
                  LobsterAST -> -- Super class spec
                  LobsterAST -> -- Sub class spec
                  ( LobsterAST, LobsterAST ) -- New super and sub class spec
mungeProcesses preProcs super sub =
  let statements = getClassStatements super
      superId = getLobsterClassId super
      subName = getClassName sub
      superName = getClassName super
      sharedProcNames = map preName preProcs
      sharedProcIds = map preIdent preProcs
      nukes = filter ( isProcDecls sharedProcNames ) statements
      moreNukes = filter ( \s -> connectionForIds s sharedProcIds ) statements
      localProcs1 = map ( \s -> connectionTos preProcs superId s ) statements
      localProcs2 = catMaybes localProcs1
      globalProcs = map ( mkConnection sub ) localProcs2
      childPort = mkPortDeclaration "child"
      transformeds = map ( transformToInternal ( Just NeutralConnection ) "child" "active" ( Just "child" ) ) globalProcs
      sourceInternals = concat ( map fstOfThree transformeds )
      externals = concat ( map thdOfThree transformeds )
      -- Create a connection from super.create -- sub.active
      create2active :: Statement = (superName, "active") === (subName, "create")
    in ( updateStatements super [ childPort ] sourceInternals [ ] ( nukes ++ moreNukes)
       , updateStatements sub [ ] [ ] ( create2active:externals ) [ ])

{-
preConnToLocalConn :: LobsterAST -> PreConnection -> LocalConnection
preConnToLocalConn lobster pre =
  let statements = getClassStatements lobster
      name = preName pre
      mstatement = find ( \s -> isProcDecl s name ) statements
      procPs = map ( \s -> isProcDecl s name ) statements
      outs = map printTree statements
      results = zip outs procPs
  in case mstatement of
       Nothing -> error ( "preConnToLocalConn( " ++ show ( length statements ) ++
                          " )\npre = " ++ show pre ++
                          "\npolicy = " ++ printTree ( policy lobster ) ++
                          "\nresults = " ++ show results )
       Just statement ->
         LocalConnection { localConn = statement
                         , localIdent = preIdent pre
                         , localName = preName pre
                         , localId = getLobsterClassId lobster
                         }
 -}
