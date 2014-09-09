{-# OPTIONS_GHC -Wall -Werror -XFlexibleInstances -XScopedTypeVariables #-}
module SCD.Lobster.Symbolic.AstUtil
    ( xVar                -- Translate a variable into an Identifier
    , mkProcClass         -- Make a process class Id
    , mkFileClass         -- Make a file class Id
    , mkProcInstance      -- Translate 'domain p = Process ();'
    , mkClassInstance     --
    , mkCreateConnection  --
    , mkConn              -- For jury rigging connections at the last minute
    , mkFileInstance      -- Translate 'domain f = File ();'
    , mkQual              -- Make qualified name from two identifiers
    , mkQualNameQ         -- Make qualified name from pair of identifiers
    , mkQualNameExpr      -- Make an expression for a qualified name
    , mkQ                 -- Shorter name for mkQualNameExpr
    , mkUnQual            -- Make an unqualified name
    , mkClassId           -- Constructor for ClassId
    , mkIdentifier        -- Constructor for Identifier
    , mkPortDeclaration   -- Constructor for a PortDeclaration statement
    , mkPortId            -- Constructor for Port Id
    , (<==)               -- Make a right to left connection
    , (==>)               -- Make a left to right connection
    , (===)               -- Make a neutral connection
    , QualifiedName       -- Pair of strings
      -- Accessors
    , getClassId          -- Class ID (if any) from a statement
    , getIdentifier       -- Identifier (if any) from a statement
    , getFileName         -- Get file name from a domain declaration
    , getNameAndId        -- Get file name and Id from a domain declaration
    , getProcessName      -- Get process name from a domain declaration
    , getProcNameAndId    -- Get process name and id from a dom declaration
      -- Predicates
    , isIdentifier        -- Is identifier for a string
    , isDomDecl           -- Is statement a domain declaration?
    , isProcDomain        -- Is statement a domain declaration for a process?
    , isFileDomain        -- Is statement a domain declaration for a file?
    , containsFile        -- Does a policy contain dom decl for specified file?
    , containsFileClass   -- Does policy contain domain decl for specified file
    , connectionTo        -- Is statement is a connection to specified file
    , connectionTos       -- Is statement is a connection to specified files
    , connectionFrom      -- Is statement is a connection from specified file
    , connectionFroms     -- Is statement is a connection from specified files
    , connectionOfFile    -- See if connection for a specified id
    , connectionOfFiles   -- See if connection for specified ids
    , isFileDecl          -- Is statement a declaration for a file specified
    , isClassDecl         -- Is statement a class declaration?
    , isFileDecls         -- Is statement a declaration for any of the files specified
    , isDecl              -- Is statement a resource declaration
    , isProcDecl          -- Is statement a declaration for a process specified
    , isProcDecls         -- Is statement a declaration for any of the processes specified
      -- Deconstructors
    , unClassId           -- Get string out of class ID
    , unPolicy            -- Get list of statements out of Policy
    , unIdentifier        -- Get string of of Identifier
      -- Transformers
    , domainDeclToCreateConnection -- process domain decl to create connection
    , transformToConnection        -- Transform connnection to a file
    , transformFromConnection      -- Transform connnection to a file
    , transformToInternal          -- Transform To connection to external / internal pair
    , transformFromInternal        -- Transform From connection to external / internal pair
    , transformConnection          -- Transform connection of a file
    , transformInternal            -- Transform connection to external / internal pair
    , connectionForId              -- If port connection matches identifier
    , connectionForIds             -- If port connection mathes identifiers
    ) where

import Lobster.Abs
import Lobster.Print ( printTree )
import Data.Maybe ( isJust, catMaybes )
import SCD.Lobster.Symbolic.Connection
    ( LocalConnection ( .. ), PreConnection ( .. ), GlobalConnection ( .. )
    , preToLocal )

-- Translate a variable to an Identifier
xVar :: String -> Identifier
xVar v = Identifier ( LIdent v )

-- Make a class ID
mkClassId :: String -> ClassId
mkClassId s = ClassId ( UIdent s )

-- Make an identifier
mkIdentifier :: String -> Identifier
mkIdentifier s = Identifier ( mkLIdent s )

-- Make a name
mkName :: String -> Name
mkName s = Ident ( mkIdentifier s )

-- Make an unqualified name
mkUnQual :: String -> QualName
mkUnQual s = UnQual ( mkName s )

-- Make an LIdent
mkLIdent :: String -> LIdent
mkLIdent s = LIdent s

-- Deconstruct the ClassId
unClassId :: ClassId -> String
unClassId ( ClassId u ) = unUIdent u

-- Deconstruct the Identifier
unIdentifier :: Identifier -> String
unIdentifier ( Identifier i ) = unLIdent i

-- Deconstruct a name
unName :: Name -> String
unName ( Ident ident ) = unIdentifier ident
unName n = error ( "unName: " ++ printTree n )

-- Deconstruct an unqualified name
unUnQual :: QualName -> String
unUnQual ( UnQual n ) = unName n
unUnQual q = error ( "unUnQual: " ++ printTree q )

-- Deconstruct the Policy
unPolicy :: Policy -> [ Statement ]
unPolicy ( Policy ss ) = ss

unUIdent :: UIdent -> String
unUIdent ( UIdent s ) = s

unLIdent :: LIdent -> String
unLIdent ( LIdent s ) = s

-- Make a process class ID
mkProcClass :: ClassId
mkProcClass = mkClassId "Process"

-- Make a file class ID
mkFileClass :: ClassId
mkFileClass = mkClassId "File"

-- Translate 'domain p = Process ();'
mkProcInstance :: String -> FilePath -> Statement
mkProcInstance v p = DomainDeclaration ( xVar v ) mkProcClass [StringExpression p]

-- Not sure yet
mkClassInstance :: String -> String -> Statement
mkClassInstance v c = DomainDeclaration ( xVar v ) (mkClassId c) []

-- Make a qualified name from two identifiers (eg "x"."y")
mkQual :: String -> String -> QualName
mkQual x y = Qual ( mkUnQual x ) ( mkName y )

-- Make a qualified name from a pair of identifiers (eg ( "x", "y" ) )
mkQualNameQ :: QualifiedName -> QualName
mkQualNameQ = uncurry mkQual

-- Make a qualified name expression from a two strings
mkQualNameExpr :: String -> String -> Expression
mkQualNameExpr "" y = QualNameExpression ( mkUnQual y )
mkQualNameExpr x  y = QualNameExpression ( mkQual x y )

-- Determine if a name matches and id
nameId :: Name -> Identifier -> Bool
nameId ( Ident i ) ident = i == ident
nameId n ident = error ( "nameId: " ++ printTree n ++ ", " ++ printTree ident )

-- Determine if a qual name matches an id
qualNameId :: QualName -> Identifier -> Bool
qualNameId ( UnQual n ) ident = nameId n ident
qualNameId ( Qual n1 n2 ) ident = qualNameId n1 ident || nameId n2 ident

-- Determine if qualified name expression matches an Id
qualNameExprId :: Expression -> Identifier -> Bool
qualNameExprId ( QualNameExpression q ) ident = qualNameId q ident
qualNameExprId e ident = error ( "qualNameExprId: " ++ printTree e ++
                                 ", " ++ printTree ident )

-- Shorter name for the same thing
mkQ :: String -> String -> Expression
mkQ = mkQualNameExpr

-- Translate 'domain f = File ();'
mkFileInstance :: String -> FilePath -> Statement
mkFileInstance v p = DomainDeclaration ( xVar v ) mkFileClass [StringExpression p]

type QualifiedName = ( String, String )

-- Make a right to left connection
(<==) :: QualifiedName -> QualifiedName -> Statement
q1 <== q2 = let n1 = mkQualNameExpr (fst q1) (snd q1)
                n2 = mkQualNameExpr (fst q2) (snd q2)
            in PortConnection [ n1 ] RightToLeftConnection [ n2 ]

-- Make a left to right connection
(==>) :: QualifiedName -> QualifiedName -> Statement
q1 ==> q2 = let n1 = mkQualNameExpr (fst q1) (snd q1)
                n2 = mkQualNameExpr (fst q2) (snd q2)
            in PortConnection [ n1 ] LeftToRightConnection [ n2 ]

-- Make a neutral connection
(===) :: QualifiedName -> QualifiedName -> Statement
q1 === q2 = let n1 = mkQualNameExpr (fst q1) (snd q1)
                n2 = mkQualNameExpr (fst q2) (snd q2)
            in PortConnection [ n1 ] NeutralConnection [ n2 ]

isDomDecl :: Statement -> Bool
isDomDecl ( DomainDeclaration _i _c _e ) = True
isDomDecl _                              = False

isProcDomain :: Statement -> Bool
isProcDomain ( DomainDeclaration _i c _e ) = unClassId c == "Process"
isProcDomain _                             = False

isFileDomain :: Statement -> Bool
isFileDomain ( DomainDeclaration _i c _e ) = unClassId c == "File"
isFileDomain _                             = False

-- Determine if an Identifier is for a given String
isIdentifier :: Identifier -> String -> Bool
isIdentifier ( Identifier lid ) s = s == unLIdent lid

-- Determine if a statement is a file declaration
isDecl :: Statement -> Bool
isDecl ( DomainDeclaration _i _c _es ) = True
isDecl _ = False

-- Determine if a statement is a class delcaration
isClassDecl :: Statement -> Bool
isClassDecl ( ClassDeclaration _cid _ids _es ) = True
isClassDecl _ = False

-- Determine if a statement is a file declaration for a specified string
isFileDecl :: Statement -> String -> Bool
isFileDecl d@( DomainDeclaration _i _c es ) fileName =
  if length es == 1 && isFileDomain d
  then case head es of
         StringExpression s -> s == fileName
         _ -> error ( "isFileDecl, not a string expression " ++ show d )
  else error ( "isFileDecl, too many expressions " ++ show d )
isFileDecl _ _ = False

-- Determine if a statement is a process declaration for a specified string
isProcDecl :: Statement -> String -> Bool
isProcDecl d@( DomainDeclaration _i _c es ) procName =
  if isProcDomain d
  then if length es == 1
       then case head es of
              StringExpression s -> s == procName
              _ -> error ( "isProcDecl, not a string expression " ++
                         printTree d )
       else error ( "isProcDecl, too many ( " ++ show ( length es ) ++
                    " ) expressions ( " ++  show ( isProcDomain d ) ++
                    " )\n" ++ printTree d )
  else False

isProcDecl _ _ = False

-- Determine if connection involves a given identifier
connectionForId :: Statement -> Identifier -> Bool
connectionForId ( PortConnection es1 _dir es2 ) ident =
   ( or ( map ( \e -> qualNameExprId e ident ) es1 ) ) ||
   ( or ( map ( \e -> qualNameExprId e ident ) es2 ) )
connectionForId _ _ = False

connectionForIds :: Statement -> [ Identifier ] -> Bool
connectionForIds statement idents =
  or ( map ( connectionForId statement ) idents )

-- Determine if a statement is a file declaration for any of the specified strings
isFileDecls :: [ String ] -> Statement -> Bool
isFileDecls fileNames d = isFileDomain d && ( or ( map ( isFileDecl d ) fileNames ) )

-- Determine if a statement is a process declaration for any of the specified strings
isProcDecls :: [ String ] -> Statement -> Bool
isProcDecls procNames d = isProcDomain d && ( or ( map ( isProcDecl d ) procNames ) )

-- Determine if the file class is for the specified Identifier
isFileClass :: Identifier -> Statement -> Bool
isFileClass ident d@( DomainDeclaration i _c _es ) = isFileDomain d && i == ident
isFileClass _ _ = False

-- Convert a process domain declaration to a connection to the create
-- port of the process declared
domainDeclToCreateConnection :: String -> Statement -> Statement
domainDeclToCreateConnection domName d@( DomainDeclaration i _cid _es ) =
  if isProcDomain d
  then let toDomName = unIdentifier i
       in ( toDomName, "active") ==> ( domName, "create" )
  else error ( "domainDeclToCreateConnection: Not a process domain declaration " ++ printTree d )
domainDeclToCreateConnection _ d =
  error ( "domainDeclToCreateConnection: Not a domain declaration " ++ printTree d )

--------------------------------------------------------------------------------
-- Now to clean up all these versions of connections. One generic version
-- that handles "To", "From"
--------------------------------------------------------------------------------
connectionOfFile :: Statement -> Identifier -> Maybe ( Statement, Identifier )
connectionOfFile c@( PortConnection es1 _dir es2 ) ident =
  if length es1 == 1 && length es2 == 1 && ( matchExpressions ident es1 ||
                                             matchExpressions ident es2 )
  then Just ( c, ident )
  else Nothing
connectionOfFile _ _ = Nothing

-- Same thing, but check if it is a connection for any of the identifiers
-- specified
connectionOfFiles :: [ Identifier ] -> Statement -> Maybe ( Statement, Identifier )
connectionOfFiles idents statement =
  let xs = filter isJust ( map ( connectionOfFile statement ) idents )
      ys = catMaybes xs
  in if null ys
     then Nothing
     else Just ( head ys )

--------------------------------------------------------------------------------
-- And new versions of the transformers
--------------------------------------------------------------------------------
transformConnection :: String -> Identifier -> Statement -> Statement
transformConnection className fileId statement =
  case statement of
    PortConnection [ es1 ] dir [ es2 ] ->
      let es1' = if matchExpr es1 fileId then changeExpr es1 className else es1
          es2' = if matchExpr es2 fileId then changeExpr es2 className else es2
      in PortConnection [ es1' ] dir [ es2' ]
    _ -> error ( "transformConnection: " ++ printTree statement )

matchExpr :: Expression -> Identifier -> Bool
matchExpr ( QualNameExpression ( Qual ( UnQual ( Ident a ) ) ( Ident _ ) ) ) ident = a == ident
matchExpr e ident = error ( "matchExpr: " ++ printTree e ++ " ... " ++ printTree ident )

changeExpr :: Expression -> String -> Expression
changeExpr ( QualNameExpression ( Qual a b ) ) prefix =
  let aString = unUnQual a
      a' = mkUnQual ( prefix ++ "." ++ aString )
  in QualNameExpression ( Qual a' b )
changeExpr s _ = error ( "changeExpr: " ++ printTree s )

transformInternal :: String -> String -> Identifier -> Statement -> ( Statement, Statement )
transformInternal port sub fileId statement =
  case statement of
    PortConnection [ es1 ] dir [ es2 ] -> -- e.g. p.active --> x.write
      let subName = mkUnQual sub
          portName = mkName port
          newPortExpr = QualNameExpression ( Qual subName portName )
          es1' = if matchExpr es1 fileId then newPortExpr else es1
          es2' = if matchExpr es2 fileId then newPortExpr else es2
      in if matchExpr es1 fileId
         then ( PortConnection [ es1' ] dir [ es2 ], PortConnection [ es2 ] dir [ es1' ] )
         else if matchExpr es2 fileId
              then ( PortConnection [ es1 ] dir [ es2' ], PortConnection [ es2' ] dir [ es1 ] )
              else error ( " transformConnection.1: " ++ printTree statement )
    _ -> error ( "transformConnection: " ++ printTree statement )

--------------------------------------------------------------------------------
-- The old versions of connection this and that
--------------------------------------------------------------------------------

-- Determine if a statement is a connection to a specified resource
connectionTo :: Statement -> Identifier -> PreConnection -> Maybe ( LocalConnection )
connectionTo c@( PortConnection es1 LeftToRightConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 &&
     matchExpressions ( preIdent pre ) es2
  then Just ( preToLocal c from pre )
  else Nothing
connectionTo c@( PortConnection es1 NeutralConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 &&
     ( matchExpressions ( preIdent pre ) es1 ||
       matchExpressions ( preIdent pre ) es2 )
  then Just ( preToLocal c from pre )
  else Nothing
connectionTo c@( PortConnection es1 BidirectionalConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 &&
     ( matchExpressions ( preIdent pre ) es1 ||
       matchExpressions ( preIdent pre ) es2 )
  then Just ( preToLocal c from pre )
  else Nothing
connectionTo c@( PortConnection es1 RightToLeftConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 && matchExpressions ( preIdent pre ) es1
  then Just ( preToLocal c from pre )
  else Nothing
connectionTo _ _ _ = Nothing

connectionTos :: [ PreConnection ] -> Identifier -> Statement -> Maybe ( LocalConnection )
connectionTos pres from statement =
  let xs = filter isJust ( map ( connectionTo statement from ) pres )
      ys = catMaybes xs
  in if null ys
     then Nothing
     else Just ( head ys )

matchExpressions :: Identifier -> [ Expression ] -> Bool
matchExpressions ident es =
  if length es == 1
  then let QualNameExpression q = head es
       in matchTargetFile ident q
  else error ( "matchExpressions: " ++ show ( length es ) )

matchTargetFile :: Identifier -> QualName -> Bool
matchTargetFile ident ( Qual ( UnQual ( Ident n1 ) ) _ ) = n1 == ident
matchTargetFile ident ( UnQual ( Ident n ) ) = n == ident
matchTargetFile ident q =
  error ( "matchTargetFile: " ++ printTree ident ++ ", " ++ printTree q )

-- Determine if a statement is a connection from a specified resource
connectionFrom :: Statement -> Identifier -> PreConnection -> Maybe ( LocalConnection )
connectionFrom c@( PortConnection es1 LeftToRightConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 &&
     matchExpressions ( preIdent pre ) es1
  then Just ( preToLocal c from pre )
  else Nothing
connectionFrom c@( PortConnection es1 NeutralConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 &&
     ( matchExpressions ( preIdent pre ) es1 ||
       matchExpressions ( preIdent pre ) es2 )
  then Just ( preToLocal c from pre )
  else Nothing
connectionFrom c@( PortConnection es1 BidirectionalConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 &&
     ( matchExpressions ( preIdent pre ) es1 ||
       matchExpressions ( preIdent pre ) es2 )
  then Just ( preToLocal c from pre )
  else Nothing
connectionFrom c@( PortConnection es1 RightToLeftConnection es2 ) from pre =
  if length es1 == 1 && length es2 == 1 && matchExpressions ( preIdent pre ) es2
  then Just ( preToLocal c from pre )
  else Nothing
connectionFrom _ _ _ = Nothing

connectionFroms :: [ PreConnection ] -> Identifier -> Statement -> Maybe ( LocalConnection )
connectionFroms pres from statement =
  let xs = filter isJust ( map ( connectionFrom statement from ) pres )
      ys = catMaybes xs
  in if null ys
     then Nothing
     else Just ( head ys )

-- Determine if a policy contains a domain declaration for a file
containsFile :: String -> Policy -> Bool
containsFile fileName ( Policy ss ) =
  let ss' = filter ( \s -> isFileDecl s fileName ) ss
  in length ss' > 0

-- Determine if the policy has a domain declaration for the file specified
containsFileClass :: Identifier -> Policy -> Bool
containsFileClass i ( Policy ss ) =
  let ss' = filter ( isFileClass i ) ss
  in length ss' > 0

-- Get Class Id from statements having a class Id (not complete yet)
getClassId :: Statement -> ClassId
getClassId ( DomainDeclaration _i c _e ) = c
getClassId s = error ( "Attempt to get class ID from " ++ show s )

-- Get Identifier from statements having an identifier (not complete yet)
getIdentifier :: Statement -> Identifier
getIdentifier ( DomainDeclaration i _c _e ) = i
getIdentifier s = error ( "Attempt to get identifier from " ++ show s )

-- Get name from a domain declaration, assumed only one expression, which
-- is a string expression identifying the name
getFileName :: Statement -> String
getFileName d@( DomainDeclaration _i _c es ) =
  if length es == 1
  then case head es of
         StringExpression s -> s
         _ -> error ( "getFileName, not a string expression " ++ show d )
  else error ( "getFileName, too many expressions " ++ show d )
getFileName d = error ( "getFileName, not a domain declaration" ++ show d )

mkCreateConnection :: GlobalConnection -> Statement
mkCreateConnection global =
  let from :: Expression
      from = mkQ ( unIdentifier ( fromClass global ) ) "create"
      to :: Expression
      to = mkQ ( unIdentifier ( toClass global ) ) "create"
  in PortConnection [ from ] LeftToRightConnection [ to ]

pactive :: Expression
pactive = mkQ "p" "active"

-- Transform an external "to" connection of a shared resource
-- We specify (maybe) a new direction for the connection, this
-- allows us to make "--" connections
-- HACK, currently es1 always should be p.active, we take
-- advantage of this to get this working now
transformToInternal :: Maybe Connection -> -- Direction of connections
                                           -- Nothing means preserve direction
                       String ->           -- Name of external port in the
                                           -- source specification
                       String ->           -- Name of external port in the
                                           -- target specification
                       ( Maybe String ) -> -- Allows changing internal port
                                           -- (hardwire) HACK
                       GlobalConnection ->
                       ( [ Statement ] -- From spec internal
                       , [ Statement ] -- To spec internal
                       , [ Statement ] ) -- External (top level)
transformToInternal newDir extNameSource extNameTarget mIntName global =
  let statement = portConn global
  in case statement of
       PortConnection [ _es1 ] dir [ es2 ] -> -- e.g. p.active --> x3.write
         let QualNameExpression ( Qual _n2a n2b ) = es2 -- so n2b = write
             intExpr = mkQ ( unIdentifier ( identTo global ) ) ( unName n2b )
             subPortExpr = mkQ ( unIdentifier ( toClass global ) ) extNameTarget
             subPortExprInt = mkQ "" extNameTarget
             superPortExpr = mkQ ( unIdentifier ( fromClass global ) ) extNameSource
             superPortExprInt = mkQ "" extNameSource
             superPortInt = case mIntName of
                              Nothing -> pactive
                              ( Just intName ) -> mkQ "p" intName
             -- Prepare p.active --> ext
             toppactive2ext :: Connection -> Statement
             toppactive2ext d = PortConnection [ superPortInt ] d [ superPortExprInt ]
--             toppactive2ext d = PortConnection [ es1 ] d [ superPortExprInt ]
             -- Prepare ext --> a.in
             ext2subport :: Connection -> Statement
             ext2subport d = PortConnection [ superPortExpr ] d [ subPortExpr ]
             -- Prepare a.in --> x7.write
             subPort2int :: Connection -> Statement
             subPort2int d = PortConnection [ subPortExprInt ] d [ intExpr ]
         in case newDir of
              Nothing ->
                case dir of
                  LeftToRightConnection ->
                     ( [ toppactive2ext LeftToRightConnection ]
                     , [ subPort2int LeftToRightConnection ]                                         , [ ext2subport LeftToRightConnection ] )
                  RightToLeftConnection ->
                     ( [ toppactive2ext RightToLeftConnection ]
                     , [ subPort2int RightToLeftConnection ]
                     , [ ext2subport RightToLeftConnection ] )
                  NeutralConnection ->
                     ( [ toppactive2ext NeutralConnection ]
                     , [ subPort2int NeutralConnection ]
                     , [ ext2subport NeutralConnection ] )
                  BidirectionalConnection ->
                     ( [ toppactive2ext BidirectionalConnection ]
                     , [ subPort2int BidirectionalConnection ]
                     , [ ext2subport BidirectionalConnection ] )
              ( Just newdir ) ->
                     ( [ toppactive2ext newdir ]
                     , [ subPort2int newdir ]
                     , [ ext2subport newdir ] )
       _ -> error ( "transformToInternal: " ++ printTree statement )

-- Transform an external "to" connection of a shared resource
transformFromInternal :: Maybe Connection -> -- Direction of connections
                                             -- Nothing means preserve direction
                         String ->           -- Name of external port in the
                                             -- source specification
                         String ->           -- Name of external port in the
                                             -- target specification
                         GlobalConnection ->
                         ( [ Statement ] -- From spec, internal
                         , [ Statement ] -- To spec, internal
                         , [ Statement ] ) -- External (top level)
transformFromInternal newDir extNameSource extNameTarget global =
  let statement = portConn global
  in case statement of
       PortConnection [ es1 ] dir [ es2 ] -> -- e.g. p.active --> x.write
         let QualNameExpression ( Qual _n2a n2b ) = es2
             intExpr = mkQ ( unIdentifier ( identTo global ) ) ( unName n2b )
             subPortExpr = mkQ ( unIdentifier ( toClass global ) ) extNameTarget
             subPortExprInt = mkQ "" extNameTarget
             superPortExpr = mkQ ( unIdentifier ( fromClass global ) ) extNameSource
             superPortExprInt = mkQ "" extNameSource
             -- Prepare p.active --> ext
             toppactive2ext :: Connection -> Statement
             toppactive2ext d = PortConnection [ es1 ] d [ superPortExprInt ]
             -- Prepare ext --> a.in
             ext2subport :: Connection -> Statement
             ext2subport d = PortConnection [ superPortExpr ] d [ subPortExpr ]
             -- Prepare a.in --> x7.write
             subPort2int :: Connection -> Statement
             subPort2int d = PortConnection [ subPortExprInt ] d [ intExpr ]
         in case newDir of
              Nothing ->
                case dir of
                  LeftToRightConnection ->
                      ( [ toppactive2ext LeftToRightConnection ]
                      , [ subPort2int LeftToRightConnection ]
                      , [ ext2subport LeftToRightConnection ] )
                  RightToLeftConnection ->
                      ( [ toppactive2ext RightToLeftConnection ]
                      , [ subPort2int RightToLeftConnection ]
                      , [ ext2subport RightToLeftConnection ] )
                  NeutralConnection ->
                      ( [ toppactive2ext NeutralConnection ]
                      , [ subPort2int NeutralConnection ]
                      , [ ext2subport NeutralConnection ] )
                  BidirectionalConnection ->
                      ( [ toppactive2ext BidirectionalConnection ]
                      , [ subPort2int BidirectionalConnection ]
                      , [ ext2subport BidirectionalConnection ] )
              ( Just newdir ) ->
                     ( [ toppactive2ext newdir ]
                     , [ subPort2int newdir ]
                     , [ ext2subport newdir ] )
       _ -> error ( "transformToConnection: " ++ printTree statement )

transformToConnection :: String -> Statement -> Statement
transformToConnection className statement =
  case statement of
    PortConnection es1 dir es2 ->
      let [ QualNameExpression ( Qual n2a n2b ) ] = es2
          [ QualNameExpression ( Qual n1a n1b ) ] = es1
          n1String = unUnQual n1a
          n2String = unUnQual n2a
          n1a' = mkUnQual ( className ++ "." ++ n1String )
          n2a' = mkUnQual ( className ++ "." ++ n2String )
          newes1 = [ QualNameExpression ( Qual n1a' n1b ) ]
          newes2 = [ QualNameExpression ( Qual n2a' n2b ) ]
      in case dir of
           LeftToRightConnection -> PortConnection es1 dir newes2
           RightToLeftConnection -> PortConnection newes1 dir es2
           -- Hack **** assume connection is *to* es2
           NeutralConnection -> PortConnection es1 dir newes2
           -- Hack **** assume connection is *to* es2
           BidirectionalConnection -> PortConnection es1 dir newes2
    _ -> error ( "transformToConnection: " ++ printTree statement )

transformFromConnection :: String -> Statement -> Statement
transformFromConnection className statement =
  case statement of
    PortConnection es1 dir es2 ->
      let [ QualNameExpression ( Qual n1a n1b ) ] = es1
          [ QualNameExpression ( Qual n2a n2b ) ] = es2
          n1String = unUnQual n1a
          n2String = unUnQual n2a
          n1a' = mkUnQual ( className ++ "." ++ n1String )
          n2a' = mkUnQual ( className ++ "." ++ n2String )
          newes1 = [ QualNameExpression ( Qual n1a' n1b ) ]
          newes2 = [ QualNameExpression ( Qual n2a' n2b ) ]
      in case dir of
           LeftToRightConnection -> PortConnection newes1 dir es2
           RightToLeftConnection -> PortConnection es1 dir newes2
           -- Hack **** assume connection is *from* es2
           NeutralConnection -> PortConnection es1 dir newes2
           -- Hack **** assume connection is *to* es2
           BidirectionalConnection -> PortConnection es1 dir newes2
    _ -> error ( "transformToConnection: " ++ printTree statement )

getNameAndId :: Statement -> ( String, Identifier )
getNameAndId d@( DomainDeclaration i _c es ) =
  if length es == 1
  then case head es of
         StringExpression s -> ( s, i )
         _ -> error ( "getFileName, not a string expression " ++ show d )
  else error ( "getFileName, too many expressions " ++ show d )
getNameAndId d = error ( "getFileName, not a domain declaration" ++ show d )

getProcessName :: Statement -> String
getProcessName = getFileName

getProcNameAndId :: Statement -> ( String, Identifier )
getProcNameAndId = getNameAndId

-- For last minute kludges, a function that makes whatever connection we want
-- Actually, now that I look at it, this is very similar to (===), sort
-- of a curried version of (===)
mkConn :: String -> String -> String -> String-> Statement
mkConn from1 from2 to1 to2 =
  let fromE :: Expression = mkQ from1 from2
      toE :: Expression = mkQ to1 to2
  in PortConnection [ fromE ] NeutralConnection [ toE ]

--------------------------------------------------------------------------------
-- Some stuff for port declarations
--------------------------------------------------------------------------------

-- Make a port ID from a string
mkPortId :: String -> PortId
mkPortId s = PortId ( LIdent s )

-- We will always use ports without constraints
mkPortDeclarationType :: PortDeclarationType
mkPortDeclarationType = EmptyPDT

-- For now not using the port declaration connections
mkPortDeclarationConnection :: PortDeclarationConnection
mkPortDeclarationConnection = EmptyPDC

mkPortDeclaration :: String -> Statement
mkPortDeclaration s
  = PortDeclaration ( mkPortId s ) mkPortDeclarationType mkPortDeclarationConnection
