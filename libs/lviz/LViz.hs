{-# OPTIONS -Wall #-}

--  -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-missing-signatures

module LViz
  ( Cfg(..)
  , LensType(..)
  , VSt(..)
  , VGraph(..)
  , PosPx
  , RGBA
  , SizePx
  , Px
  , toVGraph
  , initVSt
  , runFigVGraph
  , lookupCollision
  , fmapVGraph
  , transparency
  )
where

import Control.Monad ( liftM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( gets, runState, modify, State )
import Data.Array
import Data.Graph
import Data.List
import Data.Map (Map)
import Data.Maybe
import Graphics.Rendering.Cairo hiding (liftIO)
import Graphics.UI.Gtk.Cairo
import Graphics.Rendering.Pango.Context
import Graphics.Rendering.Pango.Font
import Graphics.Rendering.Pango.Layout
import Lobster.Abs
import Lobster.Domain hiding (empty,PortTypeValue,PortType,directionPortType)
import Lobster.Policy (ContextClass,Value,PortTypeValue, positionPortType, directionPortType, PortType(..))
import Lobster.Common
import qualified Data.Map as M
import qualified Lobster.Domain as Domain
import qualified Lobster.Print

-- import Debug.Trace

--------------------------------------------------------------------------------
-- Visualization
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

data Cfg = Cfg
  { filePaths :: [FilePath]
  , levelColors :: Array Int RGBA
  , edgeColor :: RGBA
  , lensType :: LensType
  , options :: Options
  , isWrap :: Bool
  , isCurveEdges :: Bool
  , isNoOverlap :: Bool
  } deriving (Show,Eq)

type RGBA = (Double, Double, Double, Double)

data LensType
  = StandardLens
  | StretchLens PosPx Px
  deriving (Show,Eq)

type Px = Double

type PosPx = (Px,Px)
type SizePx = (Px,Px)
type BB = (PosPx,PosPx)
type Line = (PosPx,PosPx)

data Intersection
  = Intersect PosPx
--  | Coincident
  | NoIntersect
  deriving Show

data Side = North | South | West | East deriving (Show,Eq,Read,Enum)

type NodeId = [Text]
type GraphId = [Text]

data VNode = VNode
  { side :: Side
  } deriving (Show, Eq)

data VEdge = VEdge
  { connection :: Connection
  } deriving (Show,Eq)

data VGraph = VGraph
  { graphId :: GraphId
  , msubgraphs :: [[[[VGraph]]]]
  , vnodes :: [(NodeId, VNode)]
  , vedges :: [(NodeId, NodeId, VEdge)]
  , gExpanded :: Bool
  } deriving (Show,Eq)

data Text = Text
  { textString :: String
  , textSize :: SizePx
  } deriving (Show,Eq,Ord)

data Fig = Fig
  { sizeFig :: SizePx
  , renderFig :: PosPx -> M (Render ())
  }

data VSt = VSt
  { nodeTbl :: Map NodeId PosPx
  , graphTbl :: Map GraphId BB
  , vstCfg :: Cfg
  } deriving Show

type M a = State VSt a

-----------------------------------------------------------------------
-- Defaults
-----------------------------------------------------------------------

nodeWidth :: Px
nodeWidth = 2*nodeRadius

nodeHeight :: Px
nodeHeight = 2*nodeRadius

nodeRadius :: Px
nodeRadius = 8

connectionRadius :: Px
connectionRadius = 2

smallVGraphWidth :: Px
smallVGraphWidth = 12

smallVGraphHeight :: Px
smallVGraphHeight = smallVGraphWidth

smallSep :: Px
smallSep = 2

bigSep :: Px
bigSep = nodeRadius

smallVGraphSize :: SizePx
smallVGraphSize = (smallVGraphWidth,smallVGraphHeight)

nodeSize :: (Px, Px)
nodeSize = (nodeWidth, nodeHeight)

fontString :: String
fontString = "Arial"

fontSize :: Double
fontSize = 8

transparency :: Double
transparency = 0.6

normalMarkup :: String -> String
normalMarkup = markupString "b"

expandedMarkup :: String -> String
expandedMarkup = markupString "u" . normalMarkup

unexpandedMarkup :: String -> String
unexpandedMarkup = markupString "i" . flip (++) "*" . expandedMarkup

nodeLabelOffset :: Px
nodeLabelOffset = nodeRadius + 2

-----------------------------------------------------------------------
-- Functions to convert a Domain into a VGraph
-- (makes it slightly easier to work with)
-----------------------------------------------------------------------

toVGraph :: GraphId -> Domain (ContextClass, [Value]) PortTypeValue -> IO VGraph
toVGraph pre d = do
  gid <- if null (name d)
    then return []
    else do
      nm <- toText $ name d
      return $ pre ++ [nm]
  ns <- mapM (toNode gid) $ M.toList $ ports d
  es <- mapM (toEdge d gid) $ M.toList $ connections d
  gs <- mapM (toVGraph gid) $ M.elems $ subDomains d
  return $ VGraph
    { graphId = gid
    , msubgraphs = groupVGraphs es gs
    , vnodes = ns
    , vedges = es
    , gExpanded = True -- if null (name d) then True else False
    }

type GID = String

groupVGraphs :: [(NodeId,NodeId,VEdge)] -> [VGraph] -> [[[[VGraph]]]]
groupVGraphs es gs = map (map (map (map lookupGID))) gids
  where
  tbl = concatMap (connEdge $ concatMap vnodes gs) es
  gids0 = sortBy (\a b -> compare (length b) (length a)) $
    groupConnectedVGraphs tbl $ map graphGID gs
  gids1 = map (sccVGraphs tbl) gids0
  gids = map (groupSCCVGraphs tbl) gids1
  lookupGID gid = fromMaybe (error "unknown gid") $ lookup gid [ (graphGID g, g) | g <- gs ]

groupConnectedVGraphs :: [(GID,GID)] -> [GID] -> [[GID]]
groupConnectedVGraphs tbl gs = sccVGraphs (tbl ++ map swap tbl) gs

sccVGraphs :: [(GID,GID)] -> [GID] -> [[GID]]
sccVGraphs tbl gs = map (reverse . flattenSCC) $ stronglyConnComp $ map (connVGraph tbl) gs

groupSCCVGraphs :: [(GID,GID)] -> [[GID]] -> [[[GID]]]
groupSCCVGraphs tbl gss = reverse $ sortByPoset anyPath gss
  where
  (grph,_,toMVertex) = graphFromEdges $ map (connVGraph tbl) $ concat gss
  toVertex = fromJust . toMVertex
  anyPath xs ys = or [ path grph (toVertex y) (toVertex x) | x <- xs, y <- ys ]

sortByPoset :: (a -> a -> Bool) -> [a] -> [[a]]
sortByPoset _ [] = []
sortByPoset f (x:xs) = insPoset f x $ sortByPoset f xs

insPoset :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
insPoset _ x [] = [[x]]
insPoset f x (xs:xss)
  | null b = xs : insPoset f x xss
  | null a = (x:b) : xss
  | otherwise = a : (x:b) : xss
  where
  (a,b) = partition (f x) xs

connEdge :: [(NodeId, VNode)] -> (NodeId, NodeId, VEdge) -> [(GID,GID)]
connEdge tbl (a,b,_) = case (lookup a tbl, lookup b tbl) of
  (Just n1, Just n2) -> case (side n1, side n2) of
    (East, _) -> [(nodeIdToGID b, nodeIdToGID a)]
    (West, _) -> [(nodeIdToGID a, nodeIdToGID b)]
    _ -> [(nodeIdToGID a, nodeIdToGID b),(nodeIdToGID b, nodeIdToGID a)]
  _ -> []

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

graphGID :: VGraph -> GID
graphGID = graphIdToGID . graphId

graphIdToGID :: GraphId -> GID
graphIdToGID gid = concat $ intersperse "." $ map textString gid

nodeIdToGID :: NodeId -> GID
nodeIdToGID = graphIdToGID . init

connVGraph :: [(GID,GID)] -> GID -> (GID, GID, [GID])
connVGraph xs gid = (gid, gid, [ b | (a,b) <- xs, a == gid ])

toText :: String -> IO Text
toText s = do
  (_, PangoRectangle _ _ w h) <- layoutMarkup (normalMarkup s) >>= layoutGetExtents
  return $ Text{ textString = s, textSize = (w,h) }

toNode :: [Text] -> (PortId, Domain.PortType PortTypeValue) -> IO ([Text], VNode)
toNode gid (i,t) = do
  n <- toText $ Lobster.Print.printTree i
  return (gid ++ [n], toVNode t)

toVNode :: Domain.PortType PortTypeValue -> VNode
toVNode t = VNode {side = sd}
  where
    pt = PortType t
    sd = case positionPortType pt of
           Nothing -> case directionPortType pt of
                        Just InputDirection  -> West
                        Just OutputDirection -> East
                        _ -> South
           Just SubjectPosition -> East
           Just ObjectPosition -> West

toEdge :: Domain a b -> GraphId -> ((DomainPort, DomainPort), Connection) -> IO (NodeId, NodeId, VEdge)
toEdge d gid ((a,b),c) = do
  a1 <- toNodeId d gid a
  b1 <- toNodeId d gid b
  return (a1,b1,VEdge{ connection = c })

toNodeId :: Domain a b -> GraphId -> DomainPort -> IO NodeId
toNodeId d gid0 x = do
  gid <- case domain x of
    Nothing -> return gid0
    Just y -> toText (prettyPrintDomainPortId d y) >>= \a -> return (gid0 ++ [a])
  nid <- toText $ Lobster.Print.printTree $ port x
  return $ gid ++ [nid]

-----------------------------------------------------------------------
-- Container functions
-----------------------------------------------------------------------

fmapVGraph :: (VGraph -> VGraph) -> VGraph -> VGraph
fmapVGraph f g = f $ g{ msubgraphs = map (map (map (map (fmapVGraph f)))) $ msubgraphs g }

-----------------------------------------------------------------------
-- Geometry functions
-----------------------------------------------------------------------

distance :: PosPx -> PosPx -> Double
distance (x1,y1) (x2,y2) = sqrt ((x1 - x2)**2 + (y1 -y2)**2)

areaBB :: BB -> Double
areaBB = area . sizeBB

bbCorners :: BB -> (PosPx,PosPx,PosPx,PosPx)
bbCorners ((x1,y1),(x2,y2)) = ((x1,y1),(x2,y1),(x1,y2),(x2,y2))

area :: SizePx -> Double
area (w,h) = w*h

sizeBB :: BB -> SizePx
sizeBB ((x1,y1),(x2,y2)) = (abs (x1 - x2), abs (y1 - y2))

isCollision :: PosPx -> BB -> Bool
isCollision (x,y) ((x1,y1),(x2,y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2

lookupCollision :: Ord a => PosPx -> Map a BB -> [a]
lookupCollision pos = map fst . sortByArea . M.toList . M.filter (isCollision pos)

sortByArea :: [(a,BB)] -> [(a,BB)]
sortByArea = sortBy $ \(_,a) (_,b) -> compare (areaBB a) (areaBB b)

unitSlope :: Line -> PosPx
unitSlope ((x1,y1),(x2,y2)) = ((x2 - x1)/dist,(y2 - y1)/dist)
  where
  dist = distance (x1,y1) (x2,y2)

intersectBB :: Line -> BB -> (BB, Maybe (PosPx,Side))
intersectBB l@(p1,_) bb = case sortByDist [ (p,sd) | (Intersect p, sd) <- map f xs ] of
  [] -> (bb, Nothing)
  [_] -> (bb, Nothing)
  (a:_) -> (bb, Just a)
  where
  (nw,ne,sw,se) = bbCorners bb
  xs = [((nw,ne),North),((ne,se),East),((sw,se),South),((nw,sw),West)]
  f (l1,sd) = (segmentsIntersect l l1, sd)
  sortByDist = sortBy (\(a,_) (b,_) -> compareDist p1 a b)

segmentsIntersect :: Line -> Line -> Intersection
segmentsIntersect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4))
  | den /= 0 && uA >= 0 && uA <= 1 && uB >= 0 && uB <= 1 = Intersect (x,y)
  | den == 0 && numUa == 0 && numUb == 0 = NoIntersect -- fixme:Coincident
  | otherwise = NoIntersect
  where
  den = (y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1)
  numUa = (x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)
  numUb = (x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)
  uA = numUa/den
  uB = numUb/den
  x = x1 + uA*(x2 - x1)
  y = y1 + uA*(y2 - y1)

-----------------------------------------------------------------------
-- VGraph functions
-----------------------------------------------------------------------

gDepth :: VGraph -> Int
gDepth = pred . length . graphId

isExpanded :: VGraph -> Bool
isExpanded g | null $ subgraphs g = True
             | otherwise = gExpanded g

subgraphs :: VGraph -> [VGraph]
subgraphs = concat . concatMap concat . msubgraphs

-----------------------------------------------------------------------
-- Render functions
-----------------------------------------------------------------------

circle :: Px -> PosPx -> Render ()
circle r (x,y) = do
  arc x y r 0 (2*pi)
  fill

arrow :: PosPx -> PosPx -> Render ()
arrow pos1@(x0,y0) pos2@(x1,y1) = do
  save
  translate x0 y0
  let
    aw = 6
    ah = 8
    (dx, dy) = unitSlope ((x0,y0), (x1,y1))
    d = distance pos1 pos2
  rotate $ parallel (dx, dy)
  moveTo 0 0
  lineTo d 0
  lineTo (d - aw) (ah/2)
  lineTo (d - aw) (-(ah/2))
  lineTo d 0
  fill
  restore

lineSegments :: [(Double, Double)] -> Render ()
lineSegments ((x0,y0):p1@(x1,y1):ps) = do
  moveTo x0 y0
  lineTo x1 y1
  lineSegments (p1:ps)
lineSegments _ = stroke

spline :: [PosPx] -> Render ()
spline ps@(p1@(x1,y1):p2:_) = do
  let (x,y) = midpoint p1 p2
  moveTo x1 y1
  lineTo x y
  stroke
  splineCurve ps
spline _ = return ()

splineCurve :: [PosPx] -> Render ()
splineCurve (p0:p1@(x1,y1):p2:ps) = do
  moveTo ax ay
  curveTo x1 y1 x1 y1 bx by
  stroke
  splineCurve (p1:p2:ps)
  where
  (ax,ay) = midpoint p0 p1
  (bx,by) = midpoint p1 p2
splineCurve [p1,p2@(x2,y2)] =do
  let (x,y) = midpoint p1 p2
  moveTo x y
  lineTo x2 y2
  stroke
splineCurve _ = stroke

midpoint :: PosPx -> PosPx -> PosPx
midpoint p1@(x1,y1) p2 = (x1 + dist*dx/2, y1 + dist*dy/2)
  where
  (dx,dy) = unitSlope (p1,p2)
  dist = distance p1 p2

parallel :: PosPx -> Double
parallel (0, 0) = error "unable to rotate point at origin"
parallel (x, y) = if y < 0 then -t0 else t0
  where
  t0 = acos (x / distance (0,0) (x,y))

markupString :: String -> String -> String
markupString m s = "<" ++ m ++ ">" ++ s ++ "</" ++ m ++ ">"

layoutMarkup :: String -> IO PangoLayout
layoutMarkup s = do
  ctxt <-cairoCreateContext Nothing
  fd <- contextGetFontDescription ctxt
  fontDescriptionSetFamily fd fontString
  fontDescriptionSetSize fd fontSize
  contextSetFontDescription ctxt fd
  lay <- layoutEmpty ctxt
  layoutSetMarkup lay s
  return lay

setRGBA :: (Double, Double, Double, Double) -> Render ()
setRGBA (r,g,b,a) = setSourceRGBA r g b a

renderEdges :: VGraph -> M (Render ())
renderEdges g = do
  rs <- mapM renderEdge $ vedges g
  ec <- getEdgeColor
  return $ do
    save
    setRGBA ec
    sequence_ rs
    restore

renderEdge :: (NodeId, NodeId, VEdge) -> M (Render ())
renderEdge (a,b,c) = do
  nTbl <- getNodeTbl
  case (M.lookup a nTbl, M.lookup b nTbl) of
    (Just (x1,y1), Just (x2,y2)) -> do
     gTbl <- getGraphTbl
     isCurve <- getIsCurveEdges
     isNoOver <- getIsNoOverlap
     let
       (pos1,pos2) = ((x1,y1),(x2,y2))
       pts = if isNoOver
         then toPath (M.elems gTbl) [pos1, pos2]
         else [pos1,pos2]
     let
       (posA0@(a0x,a0y):posA1:_) = pts
       (adx,ady) = unitSlope (posA0,posA1)
       posA = (a0x + adx*nodeRadius, a0y + ady*nodeRadius)
       (posB0@(b0x,b0y):posB1:_) = reverse pts
       (bdx,bdy) = unitSlope (posB0,posB1)
       posB = (b0x + bdx*nodeRadius, b0y + bdy*nodeRadius)
       pts1 = posA : init (tail pts) ++ [posB]
     return $ do
       save
       if isCurve then spline pts1 else lineSegments pts1
       case connection c of
         NeutralConnection -> do
           circle connectionRadius posA
           circle connectionRadius posB
         LeftToRightConnection -> do
           arrow posB1 posB
           circle connectionRadius posA
         RightToLeftConnection -> do
           arrow posA1 posA
           circle connectionRadius posB
         BidirectionalConnection -> do
           arrow posB1 posB
           arrow posA1 posA
       restore
    _ -> return $ return ()

-----------------------------------------------------------------------
-- VSt functions
-----------------------------------------------------------------------

initVSt :: Cfg -> VSt
initVSt cfg = VSt M.empty M.empty cfg

runFigVGraph :: VSt -> VGraph -> ((SizePx, Render ()), VSt)
runFigVGraph vst0 g = runState f $ initVSt $ vstCfg vst0
  where
  f = do
    fig <- figTopLevelVGraph (M.filterWithKey (\k _ -> length k == 1) $ graphTbl vst0) g
    r <- renderFig fig (0,0)
    return (sizeFig fig, r)

getCfg :: M Cfg
getCfg = gets vstCfg

getLevelColors :: M (Array Int RGBA)
getLevelColors = liftM levelColors getCfg

getEdgeColor :: M RGBA
getEdgeColor = liftM edgeColor getCfg

getLensType :: M LensType
getLensType = liftM lensType getCfg

getIsWrap :: M Bool
getIsWrap = liftM isWrap getCfg

getIsCurveEdges :: M Bool
getIsCurveEdges = liftM isCurveEdges getCfg

getIsNoOverlap :: M Bool
getIsNoOverlap = liftM isNoOverlap getCfg

getNodeTbl :: M (Map NodeId PosPx)
getNodeTbl = gets nodeTbl

getGraphTbl :: M (Map GraphId BB)
getGraphTbl = gets graphTbl

insertNode :: NodeId -> PosPx -> M ()
insertNode n pos = modify $ \st -> st{ nodeTbl = M.insert n pos $ nodeTbl st }

insertGraph :: GraphId -> BB -> M ()
insertGraph n exts = modify $ \st -> st{ graphTbl = M.insert n exts $ graphTbl st }

-----------------------------------------------------------------------
-- Generic Figure functions
-----------------------------------------------------------------------

vconcatl :: [M Fig] -> M Fig
vconcatl = foldr vcatl empty

vconcatr :: [M Fig] -> M Fig
vconcatr = foldr vcatr empty

vconcatc :: [M Fig] -> M Fig
vconcatc = foldr vcatc empty

hconcatr :: [M Fig] -> M Fig
hconcatr = foldr hcatr empty

-- hconcatl :: [M Fig] -> M Fig
-- hconcatl = foldr hcatl empty

hconcatc :: [M Fig] -> M Fig
hconcatc = foldr hcatc empty

empty :: M Fig
empty = rect (0,0)

rect :: SizePx -> M Fig
rect sz = return $ Fig
  { sizeFig = sz
  , renderFig = \_ -> return (return ())
  }

figText :: (String -> String) -> Text -> M Fig
figText f t = do
  return $ Fig
    { sizeFig = textSize t
    , renderFig = \(x,y) -> do
        return $ do
          moveTo x y
          liftIO (layoutMarkup (f $ textString t)) >>= showLayout
          stroke
    }

sep :: Px -> M Fig -> M Fig
sep px fa = do
  a <- fa
  let (w,h) = sizeFig a
  return $ a
    { sizeFig = (w + px*2, h + px*2)
    , renderFig = \(x,y) -> renderFig a (x + px, y + px)
    }

hcatc :: M Fig -> M Fig -> M Fig
hcatc fa fb = do
  a <- fa
  b <- fb
  let
    (w1,h1) = sizeFig a
    (w2,h2) = sizeFig b
    (w,h) = (w1 + w2, max h1 h2)
  return $ Fig
    { sizeFig = (w,h)
    , renderFig = \(x,y) -> do
        ra <- renderFig a (x, y + h/2 - h1/2)
        rb <- renderFig b (x + w1, y + h/2 - h2/2)
        return $ ra >> rb
    }

vcatc :: M Fig -> M Fig -> M Fig
vcatc fa fb = do
  a <- fa
  b <- fb
  let
    (w1,h1) = sizeFig a
    (w2,h2) = sizeFig b
    (w,h) = (max w1 w2, h1 + h2)
  return $ Fig
    { sizeFig = (w,h)
    , renderFig = \(x,y) -> do
        ra <- renderFig a (x + w/2 - w1/2, y)
        rb <- renderFig b (x + w/2 - w2/2, y + h1)
        return $ ra >> rb
    }

vcatl :: M Fig -> M Fig -> M Fig
vcatl fa fb = do
  a <- fa
  b <- fb
  let
    (w1,h1) = sizeFig a
    (w2,h2) = sizeFig b
    (w,h) = (max w1 w2, h1 + h2)
  return $ Fig
    { sizeFig = (w,h)
    , renderFig = \(x,y) -> do
        ra <- renderFig a (x, y)
        rb <- renderFig b (x, y + h1)
        return $ ra >> rb
    }

hcatr :: M Fig -> M Fig -> M Fig
hcatr fa fb = do
  a <- fa
  b <- fb
  let
    (w1,h1) = sizeFig a
    (w2,h2) = sizeFig b
    (w,h) = (w1 + w2, max h1 h2)
  return $ Fig
    { sizeFig = (w,h)
    , renderFig = \(x,y) -> do
        ra <- renderFig a (x, y + h - h1)
        rb <- renderFig b (x + w1, y + h - h2)
        return $ ra >> rb
    }

-- hcatl :: M Fig -> M Fig -> M Fig
-- hcatl fa fb = do
--   a <- fa
--   b <- fb
--   let
--     (w1,h1) = sizeFig a
--     (w2,h2) = sizeFig b
--     (w,h) = (w1 + w2, max h1 h2)
--   return $ Fig
--     { sizeFig = (w,h)
--     , renderFig = \(x,y) -> do
--         ra <- renderFig a (x, y)
--         rb <- renderFig b (x + w1, y)
--         return $ ra >> rb
--     }

vcatr :: M Fig -> M Fig -> M Fig
vcatr fa fb = do
  a <- fa
  b <- fb
  let
    (w1,h1) = sizeFig a
    (w2,h2) = sizeFig b
    (w,h) = (max w1 w2, h1 + h2)
  return $ Fig
    { sizeFig = (w,h)
    , renderFig = \(x,y) -> do
        ra <- renderFig a (x + w - w1, y)
        rb <- renderFig b (x + w - w2, y + h1)
        return $ ra >> rb
    }

figRot90 :: M Fig -> M Fig
figRot90 fa = do
  a <- fa
  let (w,h) = sizeFig a
  return $ Fig
    { sizeFig = (h,w)
    , renderFig = \(x,y) -> do
        r <- renderFig a (x,y)
        return $ do
          save
          translate x y
          rotate (pi/2)
          translate (-x) (-y-h)
          r
          restore
    }

-----------------------------------------------------------------------
-- VGraph Figure functions
-----------------------------------------------------------------------

figTopLevelVGraph :: Map GraphId BB -> VGraph -> M Fig
figTopLevelVGraph xs g = do
  figSubgraph <- figSubgraphs (figFirstLevelVGraph xs) (msubgraphs g)
  return $ Fig
    { sizeFig = sizeFig figSubgraph
    , renderFig = \(x,y) -> do
        rSubgraph <- renderFig figSubgraph (x, y)    
        rEdges <- renderEdges g
        return $ rEdges >> rSubgraph
    }

figFirstLevelVGraph :: Map GraphId BB -> VGraph -> M Fig
figFirstLevelVGraph xs g = do
  let mbb = M.lookup (graphId g) xs
  lens <- getLensType
  case (mbb,lens) of
    (Just ((x1,y1),(x2,y2)), StretchLens (minX,minY) px)
      | isSmallVGraph -> sep smallSep $ figSmallVGraph g
      where
      isSmallVGraph = x2 < minX || x1 > maxX || y2 < minY || y1 > maxY
      (maxX,maxY) = (minX + px, minY + px)
    _ -> figVGraph g

figSmallVGraph :: VGraph -> M Fig
figSmallVGraph g = do
  return $ Fig
    { sizeFig = smallVGraphSize
    , renderFig = \(x,y) -> do
        sequence_ [ insertNode n (x + smallVGraphWidth/2, y + smallVGraphHeight/2)
                    | (n,_) <- vnodes g ]
        r <- renderEdges g
        return $ do
          r
          rectangle x y smallVGraphWidth smallVGraphHeight
          stroke
    }

figVGraph :: VGraph -> M Fig -- never called on top-level graph
figVGraph g = do
  let
    westNodes = [ figWestNode i | (i,n) <- vnodes g, side n == West ]
    eastNodes = [ figEastNode i | (i,n) <- vnodes g, side n == East ]
    southNodes = [ figSouthNode i | (i,n) <- vnodes g, side n == South ]
    ifNotNull xs f = if null xs then nodeSpc else f xs
  figLbl <- figLabel g
  figSubgraph <- if isExpanded g then figSubgraphs figVGraph (msubgraphs g) else nodeSpc
  figWest <- ifNotNull westNodes vconcatl
  figEast <- ifNotNull eastNodes vconcatr
  figSouth <- nodeSpc `hcatc` (ifNotNull southNodes hconcatr) `hcatc` nodeSpc
  let
    (labelW,labelH) = sizeFig figLbl
    (subW,subH) = sizeFig figSubgraph
    (westW,westH) = sizeFig figWest
    (eastW,eastH) = sizeFig figEast
    (southW,southH) = sizeFig figSouth
    w = maximum [ labelW, westW + subW + eastW, southW ]
    h = labelH + maximum [ westH, subH, eastH ] + southH
    fig = Fig
      { sizeFig = (w,h)
      , renderFig = \(x,y) -> do
          insertGraph (graphId g) ((x+nodeRadius,y),(x+w-nodeRadius,y+h-nodeRadius))
          clrs <- getLevelColors
          rLabel <- renderFig figLbl (x + w/2 - labelW/2, y)
          let midH = y + labelH + (h - labelH - southH)/2
          rWest <- renderFig figWest (x, midH - westH/2)
          rSubgraph <- renderFig figSubgraph (x + westW, y + labelH)
          rEast <- renderFig figEast (x + w - eastW, midH - eastH/2)
          rSouth <- renderFig figSouth (x + w/2 - southW/2, y + h - southH)
          rEdges <- renderEdges g
          let
            bckgrnd = do
              save
              setRGBA $ clrs ! ((gDepth g) `mod` (rangeSize $ bounds clrs))
              rectangle (x + nodeRadius) y (w - nodeRadius*2) (h - nodeRadius)
              fill
              restore
              rectangle (x + nodeRadius) y (w - nodeRadius*2) (h - nodeRadius)
              stroke
              
          return $ bckgrnd >> rEdges >> rLabel >> rWest >> rEast >> rSouth >> rSubgraph
      }
  sep bigSep (return fig)

figLabel :: VGraph -> M Fig -- never called on top-level graph
figLabel g = do
  let
    gid = graphId g
    lbl = last gid
    fig | isExpanded g = figText expandedMarkup lbl
        | otherwise = figText unexpandedMarkup lbl
  nodeSpc `hcatc` fig `hcatc` nodeSpc

figSubgraphs :: (VGraph -> M Fig) -> [[[[VGraph]]]] -> M Fig
figSubgraphs f xs = do
  isWrp <- getIsWrap
  let g = if isWrp then boxConcat else hconcatc
  vconcatl $ map (g . map (vconcatc . map (figSccSubgraphs f))) xs

figSccSubgraphs :: (VGraph -> M Fig) -> [VGraph] -> M Fig
figSccSubgraphs f gs = vconcatc $ map (hconcatc . map f) $ wrapList gs

boxConcat :: [M Fig] -> M Fig
boxConcat xs = vconcatc $ map hconcatc $ wrapList xs

wrapList :: [a] -> [[a]]
wrapList [] = []
wrapList xs0 = loop xs0
  where
  loop xs = case splitAt i xs of
    (a,[]) -> [a]
    (a,b) -> a : loop b
  i = floor (sqrt (fromIntegral $ length xs0) :: Double)

figWestNode :: NodeId -> M Fig
figWestNode n = (rect (nodeLabelOffset, 0) `hcatc` nodeLabel n) `vcatl` figNode (-pi/2) n

figEastNode :: [Text] -> M Fig
figEastNode n = (nodeLabel n `hcatc` rect (nodeLabelOffset,0)) `vcatr` figNode (pi/2) n

figSouthNode :: [Text] -> M Fig
figSouthNode n = figNode pi n `hcatr` figRot90 (nodeLabel n `hcatc` rect (nodeLabelOffset,0))

nodeLabel :: [Text] -> M Fig
nodeLabel n = figText normalMarkup $ last n

figNode :: Double -> NodeId -> M Fig
figNode t n = do
  return $ Fig
    { sizeFig = nodeSize
    , renderFig = \(x0,y0) -> do
        let (x,y) = (x0 + nodeRadius, y0 + nodeRadius)
        insertNode n (x,y)
        return $ do
          arc x y (nodeRadius - 2) t (t + pi)
          fill
    }

nodeSpc :: M Fig
nodeSpc = rect nodeSize

-----------------------------------------------------------------------
-- Path finding
-----------------------------------------------------------------------

toPath :: [BB] -> [PosPx] -> [PosPx]
toPath bbs0 (p1:p2:ps)
  | null collisions = p1 : toPath bbs0 (p2:ps)
  | otherwise = case collisionSide of
      North
        | closestCorner == nw -> toPath bbs $ p1 : nw : p2 : ps
        | closestCorner == ne -> toPath bbs $ p1 : ne : p2 : ps
        | closestCorner == se -> toPath bbs $ p1 : ne : se : p2 : ps
        | otherwise -> toPath bbs $ p1 : nw : sw : p2 : ps

      South
        | closestCorner == nw -> toPath bbs $ p1 : sw : nw : p2 : ps
        | closestCorner == ne -> toPath bbs $ p1 : se : ne : p2 : ps
        | closestCorner == se -> toPath bbs $ p1 : se : p2 : ps
        | otherwise -> toPath bbs $ p1 : sw : p2 : ps

      East
        | closestCorner == nw -> toPath bbs $ p1 : ne : nw : p2 : ps
        | closestCorner == ne -> toPath bbs $ p1 : ne : p2 : ps
        | closestCorner == se -> toPath bbs $ p1 : se : p2 : ps
        | otherwise -> toPath bbs $ p1 : se : sw : p2 : ps

      West
        | closestCorner == nw -> toPath bbs $ p1 : nw : p2 : ps
        | closestCorner == ne -> toPath bbs $ p1 : nw : ne : p2 : ps
        | closestCorner == se -> toPath bbs $ p1 : sw : se : p2 : ps
        | otherwise -> toPath bbs $ p1 : sw : p2 : ps
  where
  xs = map (intersectBB (p1,p2)) bbs0
  noCollisions = [ bb | (bb, Nothing) <- xs ]
  collisions = [ (bb,p) | (bb, Just p) <- xs ]
  ((closestBB, (_,collisionSide)):otherColls) =
    sortBy (\(_,(a,_)) (_,(b,_)) -> compareDist p1 a b) collisions
  ((nwx,nwy),(nex,ney),(swx,swy),(sex,sey)) = bbCorners closestBB
  nw = (nwx - i, nwy - i)
  ne = (nex + i, ney - i)
  sw = (swx - i, swy + i)
  se = (sex + i, sey + i)
  i = bigSep
  (closestCorner:_) = sortBy (compareDist p2) [nw,ne,sw,se]
  bbs = [ bb | (bb,_) <- otherColls ] ++ noCollisions

toPath _ ps = ps

compareDist :: PosPx -> PosPx -> PosPx -> Ordering
compareDist p a b = compare (distance p a) (distance p b)

