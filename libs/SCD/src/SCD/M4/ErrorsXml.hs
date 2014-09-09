{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -XFlexibleInstances #-}
{- |
Module      : $Header$
Description : XML output for Shrimp errors
Copyright   : (c) Galois, Inc.

This module provides translations of Shrimp errors to an XML format
given by the DTD at
\<scd_root\>\/SCD\/src\/SCD\/Shrimp\/SLIDE\/actual.dtd 
-}

module SCD.M4.ErrorsXml           (genXmlOutput
                                  ,markerQN
                                  ,fileQN
                                  -- Begin exports for testing purposes
                                  --,test_sampleErrorsXml
                                  --,test_sampleErrorsStd
                                  --,pkSet,useId,defId,genericId
                                  -- End exports for testing purposes 
                                  )
where                            
                                 
import Data.Char                  (toLower)
import Data.Foldable              (toList)
import qualified Data.Map as M    (elems,mapWithKey)
import Data.Set                   (Set)
import SCD.SELinux.Syntax         (IsIdentifier(..),Identifier,SignedId(..))
import SCD.SELinux.PrettyPrint    (Pp, prettyPrint)

import SCD.M4.Kind                (ParameterInfo(ParameterInfo,optional,parameterKinds)
                                  ,Kind(..), ParameterKind(..), ParameterIndex(..))
import qualified SCD.M4.Kind as K (ParameterInfo(name))

import SCD.M4.Errors              (Error(..), errorsByType)
import Text.Happy.ParserMonad     (Pos(..))
import Text.XML.Light             (Node(..), Content(..), Element(..)
                                  ,QName, Attr(..), unqual, CData(..), CDataKind(..)
                                  ,ppTopElement)

import Prelude hiding (pi)

-- Begin imports for testing purposes only
import qualified Data.Map as M    (insert,empty)
import Data.Set                   (fromList)
import SCD.SELinux.Syntax         (ClassId(..), PermissionId(..), Sign(..))
import SCD.M4.Syntax              (M4Id(..))
import SCD.M4.Kind                (Fragment(..), ParameterMap)
-- End imports for testing purposes only

-- | The qualified name for the marker element.
markerQN :: QName
markerQN = unqual "marker"

-- | The qualified name for the file attribute.
fileQN :: QName
fileQN = unqual "file"

-- | Generates an XML format representation of the given Shrimp errors,
-- rendered as a string and as an XML parse tree (if errors exist). 
genXmlOutput :: [Error] -> (String, Maybe Element)
genXmlOutput [] = ("", Nothing)
genXmlOutput errs = 
    (ppTopElement shrimpNode, Just shrimpNode)
    where
      shrimpNode = mkElem (unqual "shrimp") [] elems
      elems      = [mkWarnsElem $ map mkNode errs']
      errs'      = (concat . errorsByType) errs

-- | The type class instance for creating XML nodes.  Unwraps ErrorsIn
-- for context and then builds the XML subtree recursively.
instance Node Error where
    node _ (ErrorsIn e@(InDefinition i) errs) = 
        mkErrElem e attrs children 
        where
          attrs    = [mkAttr (unqual "macro_name") (idString i)]
          children = mkMarkElemId i : [mkWarnsElem (map mkNode errs)]

    node _ (ErrorsIn e@(InImplementation (lid, mid)) errs) =
        mkErrElem e attrs children 
        where 
          attrs    = [mkAttr (unqual "layer") (idString lid),
                      mkAttr (unqual "module") (idString mid)]
          children = mkMarkElemId mid : [mkWarnsElem (map mkNode errs)]

    node _ (ErrorsIn ctx errs) = 
        case ctx of 
          WhenTunableTrue cond  -> tunable cond
          WhenTunableFalse cond -> tunable cond
          _                     -> error "Unsupported error context encountered."
        where
          tunable c = mkErrElem ctx [mkPPAttr "expr" c] [mkWarnsElem (map mkNode errs)]

    node _ e = mkErrElem e [] (mkc e)

-- | Makes the sibling children elements for the given error
mkc :: Error -> [Element]
mkc (DuplicateAccessVectorDefinition c oc) = mkUseDef c oc
mkc (DuplicateClassPermission p _c)        = [mkMarkElemId p]
mkc (DuplicateCommonDef c oc)              = mkUseDef c oc
mkc (DuplicateCommonPermission p _c)       = [mkMarkElemId p]
mkc (DuplicateDefinitions mds)             = map mkMarkElemId mds
mkc (DuplicateIdentifier i oi)             = mkUseDef i oi
mkc (DuplicateMacroDef i oi)               = mkUseDef i oi
mkc (DuplicateOccurrences _i p)            = [mkMarkElemId p]
mkc (DuplicateSymbolDeclaration i _k oi)   = mkUseDef i oi

mkc (FragmentKindError sids pks)           = [mkExpKindElem $ map mkNode (toList pks)
                                             ,mkElem (unqual "got_complex_param") [] cps]
                                             where
                                               cps = map mkNode sids

mkc (IllegalFragment s i)                  = [mkElem (unqual "fragment_text") [valueAttr s] []
                                             ,mkDefMarker i]

mkc (IllegalMacroUse i oi)                 = mkUseDef i oi

mkc (IllegalParameterUse parameterMap)     = (M.elems . M.mapWithKey make) parameterMap
                                             where
                                               make :: ParameterIndex -> Set ParameterKind -> Element
                                               make (ParameterIndex i) pks = 
                                                   mkElem (unqual "illegal_parameter") attrs kinds
                                                   where
                                                     attrs = [mkAttr (unqual "index") (show i)]
                                                     kinds = map mkNode (toList pks)
                                               
mkc (IllegalSymbolDeclarations is)         = map mkMarkElemId (toList is)
mkc (IllegalSymbolReference i is)          = [mkMarkElemId i, mkDefAtElem $ map mkMarkElemId is]
mkc (InconsistentMacroDefinitions ms)      = map mkMarkElemId ms
mkc (InconsistentSymbolDefinitions is)     = map mkMarkElemIdKind is

mkc (KindMismatch k ks i)                  = [mkMarkElemId i
                                             ,mkExpKindElem [mkNode k]
                                             ,mkElem (unqual "got_kinds") [] got_kinds]
                                             where
                                               got_kinds = map mkNode (toList ks)

mkc (MissingAccessVectorDefinition c)      = [mkMarkElemId c]
mkc (MissingModuleConfig mi)               = [mkMarkElemId mi]

mkc (ModuleIdMismatch mid fname)           = [mkElem (unqual "file_base_name") [nameAttr fname] []
                                             ,mkElem (unqual "module_name")    [nameAttr $ idString mid] []]

mkc (MutuallyRecursive mcs)                = map mkMarkElemId (concat mcs)
mkc (RefPolicyWarnCall i warnings)         = [mkMarkElemId i] ++ map mkWarnTextElem warnings
mkc (UndefinedCall i)                      = [mkMarkElemId i]
mkc (UndefinedCommonId c)                  = [mkMarkElemId c]
mkc (UndefinedIdentifier p)                = [mkMarkElemId p]
mkc (UndefinedIds ids)                     = map mkMarkElemIdKind ids
mkc (UndocumentedParameters _i pis)        = map mkNode pis
mkc (UnknownIfdefId i)                     = [mkMarkElemId i]
mkc (UnknownModuleConfigNames is)          = map mkMarkElemId (toList is)
mkc (UnusedArguments i u)                  = [mkMarkElemId i] ++ map mkNode u
mkc (WrongNumberOfArguments i pis oi)      = [mkMarkElemId' i (map mkNode pis), mkDefMarker oi]
mkc _                                      = error "Cannot make children for this type"

-- | Determines the name of the warning element for the given error.
warnName :: Error -> String
warnName (DuplicateAccessVectorDefinition _ _) = "duplicate_access_vector_def"
warnName (DuplicateClassPermission _ _)        = "duplicate_class_permission"
warnName (DuplicateCommonDef _ _)              = "duplicate_common_def"
warnName (DuplicateCommonPermission _ _)       = "duplicate_common_permission"
warnName (DuplicateDefinitions _)              = "duplicate_definitions"
warnName (DuplicateIdentifier _ _)             = "duplicate_identifier"
warnName (DuplicateMacroDef _ _)               = "duplicate_macro_def"
warnName (DuplicateOccurrences _ _)            = "duplicate_occurrences"
warnName (DuplicateSymbolDeclaration _ _ _)    = "duplicate_symbol_decl"
warnName (FragmentKindError _ _)               = "fragment_kind_error"
warnName (IllegalFragment _ _)                 = "illegal_fragment"
warnName (IllegalMacroUse _ _)                 = "illegal_macro_use"
warnName (IllegalParameterUse _)               = "illegal_parameter_use"
warnName (IllegalSymbolDeclarations _)         = "illegal_symbol_decls"
warnName (IllegalSymbolReference _ _)          = "illegal_symbol_ref"
warnName (InconsistentMacroDefinitions _)      = "inconsistent_macro_defs"
warnName (InconsistentSymbolDefinitions _)     = "inconsistent_symbol_defs"
warnName (InDefinition _)                      = "in_definition"
warnName (InImplementation _)                  = "in_implementation"
warnName (MissingAccessVectorDefinition _)     = "missing_access_vector_def"
warnName (MissingModuleConfig _)               = "missing_module_config"
warnName (ModuleIdMismatch _ _)                = "module_id_mismatch"
warnName (MutuallyRecursive _)                 = "mutually_recursive"
warnName (RefPolicyWarnCall _ _)               = "ref_policy_warn_call"
warnName (KindMismatch _ _ _ )                 = "kind_mismatch"
warnName (UndefinedCall _)                     = "undefined_macro"
warnName (UndefinedCommonId _)                 = "undefined_common_id"
warnName (UndefinedIdentifier _)               = "undefined_identifier"
warnName (UndefinedIds _)                      = "undefined_identifiers"
warnName (UndocumentedParameters _ _)          = "undocumented_parameters"
warnName (UnknownIfdefId _)                    = "unknown_ifdef_id"
warnName (UnknownModuleConfigNames _)          = "unknown_module_config_names"
warnName (UnusedArguments _ _ )                = "unused_parameters"
warnName (WhenTunableTrue _)                   = "when_tunable_true"
warnName (WhenTunableFalse _)                  = "when_tunable_false"
warnName (WrongNumberOfArguments _ _ _)        = "wrong_number_of_arguments"
warnName _                                     = error "Unsupported error type"

instance Node Kind where
    node _ k = mkElem (unqual "kind") [ppNameAttr k] [] 

instance Node ParameterInfo where
    node _ pi = mkElem (unqual "param_info") attrs kindChildren
        where
          attrs         = maybe [] (:[]) (paramNameAttr pi) ++ attrs'
          attrs'        = [mkAttr (unqual "optional") optVal]
          optVal        = map toLower . show . optional $ pi
          kindChildren  = map mkNode (pkinds pi)
          pkinds        = toList . parameterKinds
          paramNameAttr = maybe Nothing (Just . nameAttr) . K.name

instance Node ParameterKind where
    node _ pk = mkElem (unqual "kind") [ppNameAttr pk] []

instance Node (SignedId Identifier) where
    node _ sid@(SignedId _ i) = 
        mkElem (unqual "complex_param_comp") 
               [ppValueAttr sid] [mkMarkElemId i]

-- | Creates an element for the given type; assumes that the Node
-- instance for the given type for type already embeds a name suitable
-- for the type.
mkNode :: Node alpha => alpha -> Element
mkNode = node (unqual "")

-- | Attribute constructor given name/value
mkAttr :: QName -> String -> Attr
mkAttr k v = Attr{attrKey = k, attrVal = v}

-- | Produces the attribute name="value" when given the string "value"
nameAttr :: String -> Attr
nameAttr = mkAttr (unqual "name")

-- | Produces the attribute value="foo" when given the string "foo"
valueAttr :: String -> Attr
valueAttr = mkAttr (unqual "value")

-- | Same as mkAttr, but pretty-prints the value
mkPPAttr :: Pp alpha => String -> alpha -> Attr
mkPPAttr name = mkAttr (unqual name) . prettyPrint

-- | Same as nameAttr, but pretty-prints the given value
ppNameAttr :: Pp alpha => alpha -> Attr
ppNameAttr = mkPPAttr "name"

-- | Same as valueAttr, but pretty-prints the given value
ppValueAttr :: Pp alpha => alpha -> Attr
ppValueAttr = mkPPAttr "value"

-- | Constructs an XML element
mkElem :: QName -> [Attr] -> [Element] -> Element
mkElem qn attrs children = Element{ elName    = qn
                                  , elAttribs = attrs
                                  , elContent = map Elem children
                                  , elLine    = Nothing
                                  }

-- | Constructs an expected_kind element with the given elements as
-- children.
mkExpKindElem :: [Element] -> Element
mkExpKindElem = mkElem (unqual "expected_kind") [] 

-- | Constructs an XML element for top-level error types (e.g., performs
-- lookup by error type for the element name)
mkErrElem :: Error -> [Attr] -> [Element] -> Element
mkErrElem e attrs children = mkElem (unqual (warnName e)) attrs children

-- | Constructs the top-level warnings element
mkWarnsElem :: [Element] -> Element
mkWarnsElem = mkElem (unqual "warnings") []

-- | Generates a marker element, e.g.
--   <marker file="file" name="name" line="linenum" col="colnum" ../>
mkMarkElem :: String -> Pos -> [Attr] -> [Element] -> Element
mkMarkElem name (Pos file sc line col) extraAttrs children = 
    mkElem markerQN attrs children
    where
      attrs       = attrs' ++ extraAttrs
      attrs'      = [fileAttr file, nameAttr name, 
                    lineAttr line, colAttr col,
                    offsetAttr sc, eOffsetAttr ec]
      ec          = sc + length(name)
      fileAttr    = mkAttr fileQN
      lineAttr    = mkAttr (unqual "line")         . show
      colAttr     = mkAttr (unqual "col")          . show
      offsetAttr  = mkAttr (unqual "start_offset") . show
      eOffsetAttr = mkAttr (unqual "end_offset")   . show

mkMarkElemId :: IsIdentifier alpha => alpha -> Element
mkMarkElemId i = mkMarkElemId' i []

mkMarkElemId' :: IsIdentifier alpha => alpha -> [Element] -> Element
mkMarkElemId' i children = mkMarkElemId'' i [] children

mkMarkElemId'' :: IsIdentifier alpha => alpha -> [Attr] -> [Element] -> Element
mkMarkElemId'' i attrs children = mkMarkElem (idString i) (pos i) attrs children

-- | Generates a marker with a nested kind sub-element
mkMarkElemIdKind :: IsIdentifier alpha => (alpha, Kind) -> Element
mkMarkElemIdKind (i,k) = mkMarkElem (idString i) (pos i) [] [mkNode k]

-- | Generates a defined_at element.
mkDefAtElem :: [Element] -> Element
mkDefAtElem = mkElem (unqual "defined_at") [] 

-- | Generates a defined_at tree surrounding a marker, e.g.
-- <defined_at> <marker ...> .. <marker ...> </defined_at>
mkDefMarker :: IsIdentifier alpha => alpha -> Element
mkDefMarker i = mkDefAtElem [mkMarkElemId i]

-- | Generates a marker for a simple use/def pair, that is, the use marker and a
-- defined_at sibling with the def marker element in its subtree, e.g.:
--   <marker file="use_file" name="use_name" line="use_linenum" ../>
--   <defined_at>
--     <marker file="def_file" name="def_name" line="def_linenum" ../>
--   </defined_at>
mkUseDef :: (IsIdentifier alpha, IsIdentifier beta) => alpha -> beta -> [Element]
mkUseDef use def = [mkMarkElemId use, mkDefMarker def]

-- | Generates a warning_text element, e.g., 
-- <warning_text>Some Warning Text</warning_text>
mkWarnTextElem :: String -> Element
mkWarnTextElem text = 
    Element { elName    = unqual "warning_text"
            , elAttribs = []
            , elContent = [Text $ CData { cdVerbatim = CDataText, 
                                          cdData     = text, 
                                          cdLine     = Nothing 
                                        }]
            , elLine    = Nothing
            }

--------------------------------------------------------------------------------
-- Test gunk
--------------------------------------------------------------------------------

-- | Testing purposes only
genericId :: IsIdentifier alpha => alpha
genericId = mkId' (Pos "generic_filename" 0 0 0) "generic_id"

-- | Testing purposes only
useId :: IsIdentifier alpha => alpha
useId = mkId' (Pos "use_filename" 0 1 2) "use_identifier"

-- | Testing purposes only
defId :: IsIdentifier alpha => alpha
defId = mkId' (Pos "def_filename" 0 3 4) "def_identifier"

-- | Testing purposes only
inImpl :: Error
inImpl = InDefinition (mkId' (Pos "implementation_file" 0 5 6) "my_macro_name")

-- | Testing purposes only
pkSet :: Set ParameterKind
pkSet = fromList [ParameterKind { fragment = Whole, kind = AnyKind }
                 ,ParameterKind { fragment = Whole, kind = ClassKind }]


-- | Testing purposes only
pmap :: ParameterMap
pmap = let m1 = M.insert (ParameterIndex 0) pkSet M.empty
           m2 = M.insert (ParameterIndex 1) pkSet m1
           m3 = M.insert (ParameterIndex 2) pkSet m2
       in 
         m3

-- | Testing purposes only
testErrors :: [Error]
testErrors = 
    map wrapErrors
      [
      DuplicateAccessVectorDefinition (ClassId useId) (ClassId defId)
    , DuplicateClassPermission (PermissionId genericId) (ClassId defId)
    , DuplicateDefinitions (replicate 4 (M4Id genericId))
    , IllegalSymbolDeclarations (fromList [ useId, defId ])
    , IllegalSymbolReference (genericId) [   ]
    , IllegalSymbolReference (genericId) [ useId, defId  ]
    , InconsistentSymbolDefinitions [ (useId, ClassKind), (defId, PermissionKind) ]
    , MutuallyRecursive [ [ M4Id useId, M4Id defId ], [ M4Id genericId, M4Id genericId ] ]
    , WrongNumberOfArguments 
      (M4Id useId) 
      [
        ParameterInfo { K.name = Just "param1", optional = False, parameterKinds = pkSet }
      , ParameterInfo { K.name = Just "param2", optional = False, parameterKinds = pkSet }
      , ParameterInfo { K.name = Nothing, optional = False, parameterKinds = pkSet }
      ]
      defId
    , KindMismatch ClassKind (fromList [PermissionKind,UserKind]) genericId
    , UnusedArguments (M4Id genericId)
        [ParameterInfo {K.name         = Just "domain", 
                        optional       = True,
                        parameterKinds = 
                          fromList
                            [ParameterKind {fragment = Whole, 
                                            kind = AnyKind}]}]
    , FragmentKindError [SignedId Negative genericId, SignedId Positive useId] pkSet
    , IllegalFragment "fragment_text" genericId
    , IllegalParameterUse pmap
    , RefPolicyWarnCall genericId ["Warning One Text", "Warning Two Text"]
     ]
    where
    wrapErrors e = ErrorsIn inImpl [e]      

-- | Testing purposes only
_test_sampleErrorsXml :: IO ()
_test_sampleErrorsXml = putStrLn . fst . genXmlOutput $ testErrors 
      
-- | Testing purposes only
_test_sampleErrorsStd :: IO ()
_test_sampleErrorsStd = putStrLn . unlines . map prettyPrint $ testErrors
--                       concat . errorsByType $ testErrors
