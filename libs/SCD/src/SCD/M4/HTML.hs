{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{- |
Module      : $Header$
Description : HTML generation from Shrimp
Copyright   : (c) Galois, Inc.

HTML generation from Shrimp
-}

module SCD.M4.HTML(HTMLOptions(..), defaultHTMLOptions, initializeDir,
  genHTML) where

import Text.XML.Light(Node, Content(..), CData(..), CDataKind(..),
  Element, QName, Attr(..), unode, unqual, ppTopElement)

import SCD.M4.Syntax(Policy, PolicyModule, policyModules, layerModule,
  LayerModule, interface, implementation, supportDefs, SupportDef(..),
  Interface(..), InterfaceElement(..), InterfaceDoc, M4Id,
  infoFlow, moduleSummary, moduleDescription, interfaceSummary,
  interfaceDescription, parameters, Parameter, parameterSummary,
  Implementation(..), required, requiredDescription, InfoFlow, flow,
  weight, Version(..))

import SCD.SELinux.Syntax(idString, toId, Identifier)
import SCD.M4.Errors(Error)

import SCD.M4.KindCheck(KindInfo, KindMaps, InterfaceEnv, m4Env,
  M4Info(..), ParameterIndex, parameterInfo,
  refpolicywarn, KindMap, kindMaps, Kind(..), inputMap, iInputMap,
  outputMap, iOutputMap, localMap, simpleKindSet, implEnv,
  simpleParameterKind, simpleParameterInfo, normalizeId, xrefs, IdXrefMap)

import SCD.M4.KindCheckPolicy(errorDoc)

import SCD.M4.Kind(getKind, ParameterInfo(..), ppParameters)
import SCD.M4.Options(Options)
import SCD.M4.Util

import SCD.M4.PShow(pShow)
import SCD.M4.PrettyPrint(interfaceTypeString)
import Text.PrettyPrint.HughesPJ(render, (<+>), (<>), parens)
import Text.PrettyPrint.Pp(pp, sepWithCommas)
import Data.Foldable(toList)
import Data.Map(lookup, assocs)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.MapSet as MapSet
import Prelude hiding(lookup)

import Data.List.GroupSort(sortProj)
import qualified Data.List as List
import System.Directory(removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath((</>))
import System.IO.Error(isDoesNotExistError)
import Control.Arrow(second)

data Hs = Hs FilePath Element
  deriving Show

data HTMLOptions = HTMLOptions{ sort :: Bool }
  deriving Show

defaultHTMLOptions :: HTMLOptions
defaultHTMLOptions = HTMLOptions{ sort = True }

tryRemoveDirectoryRecursive :: FilePath -> IO ()
tryRemoveDirectoryRecursive d =
  removeDirectoryRecursive d `catch` \e ->
  if isDoesNotExistError e then return () else ioError e

-- | Wipe a directory recursively and recreate it.

initializeDir :: FilePath -> IO ()
initializeDir d = do
  tryRemoveDirectoryRecursive d
  createDirectoryIfMissing True d

-- | This will generate HTML and put in the given directory.

genHTML :: Options -> HTMLOptions -> Policy -> KindInfo -> [Error] -> FilePath -> IO ()
genHTML os hos p ki errs d = do
  writeFile (d </> styleLink) style
  let hs = generate os hos p ki errs
  mapM_ (writeHTML d) hs

style :: String
style = unlines
  [ "table { border-collapse: collapse }"
  , "th { font-weight: normal; color: gray }"
  , "th, td { border-style: solid; border-width: thin; border-color: gray; padding: 0.25em }"
  , ".summary { font-style: italic }"
  , ".outputref { font-style: italic }"
  , ".infoflow {  }"
  , ".parameterindex {  }"
  , ".parametername { font-weight: bold }"
  , ".header { color: gray }"
  , ".warnings { background-color: #e79d76 }"
  , ".template { border-width: medium; border-style: solid; border-color: #dfed80 }"
  , ".template .banner { background-color: #dfed80; padding-top: 1px; padding-bottom: 1px; }"
  , ".interface { border-width: medium; border-style: solid; border-color: #afcad6 }"
  , ".interface .banner { background-color: #afcad6; padding-top: 1px; padding-bottom: 1px; }"
  , ".template, .interface { padding: 0.25em; margin-top: 1em }"
  , "tr.even { background-color: #dfddc0 }"
  , "tr.odd { background-color: #dfddb0 }"
  , "a:link, a:visited { text-decoration: none }"
  ]

writeHTML :: FilePath -> Hs -> IO ()
writeHTML d (Hs fp e) = writeFile (d </> fp) (ppTopElement e)

generate :: Options -> HTMLOptions -> Policy -> KindInfo -> [Error] -> [Hs]
generate os hos p ki errs =
     genErrors os errs ++
     genM4Index p ki :
     toc p errs :
     genSupportDefs hos ki (supportDefs p) ++
     map (genModule hos xm ki) (policyModules p)
  where xm = xrefs ki

genM4Index :: Policy -> KindInfo -> Hs
genM4Index p ki =
  html m4IndexLink "Macro definition index" $
    concatMap (uncurry (genSynopsis ki))
      (List.sort
        (concat [ [ (i,Just (layerModule m)) | i <- map elementId (interfaceElements (interface m)) ]
               |  m <- policyModules p ] ++
         [(i,Nothing) | SupportDef i _ <- supportDefs p]))

tocM4Index :: [Content]
tocM4Index = [ link m4IndexLink [text "Macro definition index"], br ]

toc :: Policy -> [Error] -> Hs
toc p errs  = html "index.html" "SELinux Reference Policy" $
               ((header 3 [text "Supplementary definitions"] ) :
	          tocErrors errs ++
                  tocM4Index ++
                  tocSupport (supportDefs p) ++
		tocModules (sortProj moduleName (policyModules p)))

tocModules :: [PolicyModule] -> [Content]
tocModules ps =
 [ header 3 [ divc "summary" $ text "Reference policy modules" ]
 , table
      (map ((:[]) . text) subs)
      rs
 ]
 where
   -- find all subsystem categories:
  subs = map (getSub.head) ss
   -- split them up according to subsystem category:
  ss   = List.groupBy sameSub ps
  ls   = map length ss
  mx   = maximum ls
    -- pad out each column to max number of entries
  ss1  = map (\ col -> padTo mx [] (map dropSub col)) ss
    -- generate rows by taking one from each column
  rs   = genRows ss1

  getSub a = takeWhile (/='/') (idString $ toId $ fst $ layerModule a)

    -- show module name sans its layer (that's in the column header)
  dropSub pol = tocLayerModule (layerModule pol)

  sameSub a b = getSub a == getSub b

  genRows [] = []
  genRows ([]:_) = []
  genRows rws = (map head rws) : genRows (map tail rws)

  padTo 0 _ xs = xs
  padTo n d [] = d : padTo (n-1) d []
  padTo n d (x:xs) = x : padTo (n-1) d xs

tocErrors :: [Error] -> [Content]
tocErrors [] = []
tocErrors _ = [ link errorLink [text "Translation errors"], br ]

genErrors :: Options -> [Error] -> [Hs]
genErrors _ []    = []
genErrors os errs = [ html errorLink "Reference Policy Errors" $
                        [ pre [text ((render.fst) (errorDoc os errs)) ] ]
                    ]

tocSupport :: [SupportDef] -> [Content]
tocSupport [] = []
tocSupport _ = [ link supportLink [text "Support definitions"], br ]

{-
tocModule :: PolicyModule -> [Content]
tocModule m = [link (moduleLink m) [text (moduleName m), br]]
-}

tocLayerModule :: LayerModule -> [Content]
tocLayerModule m = [link (layerModuleLink m) [text (render $ pp $ snd m)]]

moduleName :: PolicyModule -> String
moduleName = render . pp . layerModule

genModule :: HTMLOptions -> IdXrefMap -> KindInfo -> PolicyModule -> Hs
genModule hos xm ki m =
  html (moduleLink m) (moduleName m) $
     header 2 [ spanc "header" (text "module ")
               , text " "
               , text (idString mi++" ("++v++")")
               ] :
     header 2 [ spanc "header" (text "Synopsis") ] :
     genInterfaces hos xm ki (interface m) ++
     genImplementation xm ki (implementation m)

  where Implementation mi (Version v) _ = implementation m

genSynopsis :: KindInfo -> M4Id -> Maybe LayerModule -> [Content]
genSynopsis ki i mlm = [ divc "synopsis" $
                         lnk mlm
                           (text (render (pp i <>
                                         ppParameters (map simpleParameterInfo
                                           (parameterInfo (findInterfaceEnv ki i))))) ) ]
  where lnk (Just lm) = interfaceLink lm (Just i)
        lnk Nothing   = id

genInterfaces :: HTMLOptions -> IdXrefMap -> KindInfo -> Interface -> [Content]
genInterfaces hos xm ki (InterfaceModule md is) =
     [ header 1 [ divc "summary" $ text (moduleSummary md) ]
     , par "description" $ moduleDescription md
     , par "required" [
         text ("("++(if (required md) then "" else "not ")++"required"
                  ++(if (null (requiredDescription md)) then ")"
                     else ": "++requiredDescription md++")")) ]
     ]
  ++ concatMap (genInterfaceElement xm ki) (orderInterfaces hos is)

orderInterfaces :: HTMLOptions -> [InterfaceElement] -> [InterfaceElement]
orderInterfaces hos is = if sort hos then sortProj elementId is else is

genInterfaceElement :: IdXrefMap -> KindInfo -> InterfaceElement -> [Content]
genInterfaceElement xm ki (InterfaceElement it d m _) =
  divc ("interfacebody "++s) $
     [ divc "banner"
         [header 2 [ anchor (interfaceAnchor (Just m)) [
                     spanc "header" (text s), text " ", text (idString m)] ]]
     , par "warnings" $ genInterfaceWarnings (refpolicywarn ke)
     , par "interfacesummary summary" [ text (interfaceSummary d) ]
     , par "description" $ interfaceDescription d
     , par "infoflow" $ genInfoFlow (infoFlow d)
     , par "parameters" $ genParameterTable ke (parameters d)
     , genAllKindMaps xm (kindMaps ke)
     ]
  where ke = findInterfaceEnv ki m
        s = interfaceTypeString it

findInterfaceEnv :: KindInfo -> M4Id -> InterfaceEnv
findInterfaceEnv ki m =
  case lookup (toId m) (m4Env ki) of
    Just (M4Macro ke) -> ke
    _                 -> error $ "findInterfaceEnv: missing kindinfo for "++idString m

genInterfaceWarnings :: [String] -> [Content]
genInterfaceWarnings ws | not (null ws) =
  divc "header" (text "Warning:") :
  map (divc "warning" . text) ws
genInterfaceWarnings _ = []

genParameterTable :: InterfaceEnv -> [Parameter] -> [Content]
genParameterTable _ [] = []
genParameterTable ke ps =
  [ table
      (map ((:[]) . text) [ "index", "name", "kind", "summary" ])
      (map genParameter (zip3 [1..] (parameterInfo ke) (map Just ps++repeat Nothing)))
  ]

genParameter :: (ParameterIndex, ParameterInfo, Maybe Parameter) -> [[Content]]
genParameter (n, p,mp) =
     [ [ divc "parameterindex" $ text (render (pShow n)) ]
     , [ divc "parametername"  $ text (maybe "" id (name p))] ++ if optional p then [text " (optional)"] else []
     , [ divc "kind" $ text ((render . sepWithCommas .map pp . toList . simpleParameterKind)
                               (parameterKinds p)) ]
     , [ divc "parametersummary summary" $ text (maybe "" parameterSummary mp) ]
     ]

genInfoFlow :: (Maybe InfoFlow) -> [Content]
genInfoFlow Nothing = []
genInfoFlow (Just f) =
  [ spanc "header" $ text "information flow:"
  , text (render (pp (flow f)))
  , text ("("++show (weight f)++")")
  ]

data XrefInfo = NoXref | OriginXref | AllXref
  deriving (Eq, Ord, Show)

genAllKindMaps :: IdXrefMap -> KindMaps -> Content
genAllKindMaps xm km =
      par "kindmap" $
         genKindMaps xm [ ("direct input", inputMap km, AllXref)
                        , ("indirect input", iInputMap km, AllXref)
                        , ("direct output",   outputMap km, NoXref)
                        , ("indirect output", iOutputMap km, OriginXref)
                        , ("require", localMap km, AllXref)
                        ]

genKindMaps :: IdXrefMap -> [(String, KindMap, XrefInfo)] -> [Content]
genKindMaps xm ms | null rs = []
                  | otherwise =
  [ table [ divc "header" (text s) :: Content | s <- hs ] rs ]
  where rs = concatMap (genKindMap xm) ms
        hs = take (maximum (map length rs)) ["", "identifier", "kind", "origin"]

genKindMap :: IdXrefMap -> (String, KindMap, XrefInfo) ->
              [[([Attr],[Content])]]
genKindMap xm (s,km, xi) | not (Map.null km) =
  (([Attr (uq "rowspan") (show (Map.size km))],
    divc "header" (text s)) : r1) : rn
  where r1:rn = map (genKindEntry xm xi) (map (second (Set.map getKind))
                                              (assocs km))
genKindMap _ _ = []

genKindEntry :: IdXrefMap
             -> XrefInfo
	     -> (Identifier, Set Kind)
	     -> [([Attr],[Content])]
genKindEntry xm xi (i, ks) =
  [ ([], [ text (idString i) ])
  , ([], [ text ((render . sepWithCommas . map pp . toList . simpleKindSet) ks) ])
  , ([], xr)
  ] -- ++ if not (null xr) then [ ([], xr) ] else []
  where xr = concatMap (genXrefs xi)
               (toList (MapSet.lookup (normalizeId i) xm))

genXrefs :: XrefInfo -> (Identifier, LayerModule, Bool, Kind, Maybe M4Id) -> [Content]
genXrefs xi (_, lm, orm, k, mi)
  | k /= RoleKind && xi >= (if orm then OriginXref else AllXref) =
    [ interfaceLink lm mi $
        spanc (if orm then "originref" else "outputref") [
          text (render (i <+> parens (pp k))) ] ]
  | otherwise = []
  where i = maybe (pp lm) pp mi


genSupportDefs :: HTMLOptions -> KindInfo -> [SupportDef] -> [Hs]
genSupportDefs _ _ [] = []
genSupportDefs hos ki ds =
  [ html supportLink "Support definitions" $
      header 1 [ spanc "header" (text "Support definitions") ] :
      concatMap (genSupportDef ki)
                (if sort hos then sortProj (\(SupportDef i _) -> i) ds else ds) ]

genSupportDef :: KindInfo -> SupportDef -> [Content]
genSupportDef ki (SupportDef m _) =
     [ header 2 [ text (idString m) ]
     , genAllKindMaps Map.empty (kindMaps (findInterfaceEnv ki m))
     ]

genImplementation :: IdXrefMap -> KindInfo -> Implementation -> [Content]
genImplementation xm ki (Implementation mi _ _) =
    [ header 2 [ anchor (interfaceAnchor Nothing) [
                   spanc "header" (text "implementation") ] ]
    , genAllKindMaps xm km
    ]
  where km =
          maybe
	    (error $ "genImplementation: missing kindinfo for "++idString mi)
	    id
	    (lookup mi (implEnv ki))

-- links

errorLink :: String
errorLink = "Errors.html"

m4IndexLink :: String
m4IndexLink = "m4index.html"

supportLink :: String
supportLink = "Support.html"

styleLink :: String
styleLink = "style.css"

moduleLink :: PolicyModule -> String
moduleLink p = layerModuleLink (layerModule p)

layerModuleLink :: LayerModule -> String
layerModuleLink (l,m) = (idString l ++ "_"++ idString m++".html")

interfaceLink :: LayerModule -> Maybe M4Id -> Content -> Content
interfaceLink lm mi c = link (layerModuleLink lm ++"#"++ interfaceAnchor mi) [c]

interfaceAnchor :: Maybe M4Id -> String
interfaceAnchor (Just i) = idString i
interfaceAnchor Nothing  = "_implementation"

-- markup

link :: String -> [Content] -> Content
link r cs = Elem $ unode "A" ([Attr (uq "href") r], cs)

anchor :: String -> [Content] -> Content
anchor r cs = Elem $ unode "A" ([Attr (uq "name") r], cs)

html :: String -> String -> [Content] -> Hs
html f t cs = Hs f $ unode "html"
                      [ Elem (unode "head"
                         [ unode "title" (text t)
                         , unode "link" [ Attr (uq "rel") "stylesheet"
                                        , Attr (uq "href") styleLink
                                        , Attr (uq "type") "text/css"
                                        ]
                         ])
                      , Elem (unode "body" (cs++[footer]))
                      ]
 where
  footer = Elem $
            unode "div"
                  ([Attr (uq "align") "center"],
                   [ link "index.html" [text "Policy Index"]])

uq :: String -> QName
uq = unqual

text :: String -> Content
text s = Text $ CData{ cdVerbatim = CDataText, cdData = s, cdLine = Nothing }

br :: Content
br = Elem (unode "br" ())

pre :: [Content] -> Content
pre c = Elem (unode "pre" c)

table' :: (Node a, Node b) => [Attr] -> [a] -> [([Attr],[b])] -> Content
table' ha hs rs =
  Elem (unode "table"
         ( [ unode "tr" (ha, map (unode "th") hs) ]
        ++ [ unode "tr" (c:a, map (unode "td") r) | ((a,r),c) <- zip rs even_odds ] ))
 where
  even_odds = map (Attr (uq "class")) $ cycle ["even", "odd"]

table :: (Node a, Node b) => [a] -> [[b]] -> Content
table hs = table' [] hs . map (\a -> ([],a))

header :: Integer -> [Content] -> Content
header i c = Elem (unode ("h"++show i) c)

nodec :: Co a b => String -> String -> a -> b
nodec n cl = co $ \c -> if null c then text ""
                        else Elem (unode n ([Attr (uq "class") cl], c))

divc :: Co a b => String -> a -> b
divc = nodec "div"

spanc :: Co a b => String -> a -> b
spanc = nodec "span"

par :: String -> [Content] -> Content
par _ [] = text ""
par s cs = Elem (unode "p" (divc s cs :: Content))

-- class supporting flexible use of content transformers

class Co a b where
  co :: ([Content] -> Content) -> a -> b

instance Co Content Content     where co f = f . (:[])
instance Co [Content] Content   where co f = f
instance Co [Content] [Content] where co f = (:[]) . f
instance Co Content [Content]   where co f = (:[]) . f . (:[])

