{-# OPTIONS -Wall -XForeignFunctionInterface #-}
--  -fno-warn-missing-signatures

module GUI (guiMain,initCfg) where

import Control.Concurrent
import Control.Monad.State
import Data.Array
import Data.List
import Data.Maybe
import GHC.IOBase
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk ( AttrOp((:=))
                       , aboutDialogSetVersion, afterRangeValueChanged
                       , castToAboutDialog, castToButton, castToCheckMenuItem
                       , castToColorSelectionDialog, castToDialog, castToDrawingArea
                       , castToFileChooserDialog, castToHScale, castToMenu,
                         castToMenuItem, castToMessageDialog, castToRadioMenuItem
                       , castToScrolledWindow, castToViewport
                       , castToWindow, checkMenuItemGetActive, Color(Color)
                       , colorSelectionDialogGetCancelButton, colorSelectionDialogGetColor
                       , colorSelectionDialogGetHelpButton, colorSelectionDialogGetOkButton
                       , colorSelectionGetCurrentColor, colorSelectionSetCurrentColor
                       , ConnectId, Dialog, drawableGetSize, DrawingArea, DrawWindow
                       , drawWindowInvalidateRect, fileChooserAddFilter, fileChooserGetFilename
                       , fileFilterAddPattern, fileFilterNew, fileFilterSetName
                       , HScale, initGUI, mainGUI, mainQuit, Menu, menuPopup, messageDialogText
                       , onActivateLeaf, onButtonPress, onClicked, onDestroy, onExpose
                       , onFileActivated, onKeyPress, onResponse, onToggle, rangeGetValue
                       , renderWithDrawable, ResponseId(ResponseUser), set, ScrolledWindow
                       , FileChooserDialog, ColorSelectionDialog, CheckMenuItemClass
                       , MessageDialog, Viewport, widgetDestroy, widgetGetDrawWindow
                       , widgetHide, widgetSetSizeRequest, widgetShowAll, Window
                       , windowSetTitle)
import Graphics.UI.Gtk.Gdk.Events -- deprecated, but probably not a trivial conversion.
import Graphics.UI.Gtk.Glade
import LViz
import Lobster.Common
import Lobster.Version
import System.Process
import qualified Control.Exception as CE
import qualified Data.Map as M
import qualified Lobster.Policy as Policy
import Foreign
-- import ExtTools ( getInstallDataFileName_ )
import Paths_lviz ( getDataFileName )

-- import Debug.Trace

-- todo: override defaults from config file
initCfg :: Options -> [FilePath] -> Cfg
initCfg opts fns = Cfg
  { filePaths = fns
  , options = opts
  , edgeColor = toCols [0,0,0]
  , levelColors = listArray (0,3)
      [ toCols [225, 245, 255]
      , toCols [209, 255, 209]
      , toCols [255, 255, 204]
      , toCols [255, 229, 217]
      ]
  , lensType = StandardLens
  , isWrap = False
  , isCurveEdges = False
  , isNoOverlap = False
  }
  where
  toCols :: [Int] -> RGBA
  toCols xs = (r,g,b,transparency) where [r,g,b] = map (\x -> fromIntegral x / 255) xs

data St = St
  { stCfg :: Cfg
  , stVSt :: VSt
  , stRender :: Render ()
  , standardSize :: SizePx
  , mVGraph :: Maybe VGraph
  , drawingArea :: DrawingArea
  , scrolledWindow :: ScrolledWindow
  , viewport :: Viewport
  , window :: Window
  , eventXY :: PosPx
  , popupMenu :: Menu
  , fileChooserDialog :: FileChooserDialog
  , lensDialog :: Dialog
  , colorSelectionDialog :: ColorSelectionDialog
  , errorDialog :: MessageDialog
  , colorIndex :: ColorIndex
  , lensScale :: HScale
  }

guiMain :: Cfg -> IO ()
guiMain cfg0 = do
  initGUI
  gladeFn <- getDataFileName "lviz.glade"
  helpFn <- getDataFileName "help/overview.html"
  mxml <- xmlNew gladeFn
  let xml = fromMaybe (error $ "unable to load:" ++ gladeFn) mxml
  win <- xmlGetWidget xml castToWindow "window"
  scrolledwindow <- xmlGetWidget xml castToScrolledWindow "scrolledwindow"
  view <- xmlGetWidget xml castToViewport "viewport"
  popupmenu <- xmlGetWidget xml castToMenu "popupmenu"

  drawingarea <- xmlGetWidget xml castToDrawingArea "drawingarea"
  openMenuItem <- xmlGetWidget xml castToMenuItem "openMenuItem"
  editLens <- xmlGetWidget xml castToMenuItem "editLens"
  quitMenuItem <- xmlGetWidget xml castToMenuItem "quitMenuItem"
  closeMenuItem <- xmlGetWidget xml castToMenuItem "closeMenuItem"
  aboutMenuItem <- xmlGetWidget xml castToMenuItem "aboutMenuItem"
  helpMenuItem <- xmlGetWidget xml castToMenuItem "helpMenuItem"

  expandAllMenuItem <- xmlGetWidget xml castToMenuItem "expandAllMenuItem"
  collapseAllMenuItem <- xmlGetWidget xml castToMenuItem "collapseAllMenuItem"

  expandOrCollapsePopupItem <- xmlGetWidget xml castToMenuItem "expandOrCollapse"

  standardlens <- xmlGetWidget xml castToRadioMenuItem "standardlens"
  stretchlens <- xmlGetWidget xml castToRadioMenuItem "stretchlens"

  wrapmenuitem <- xmlGetWidget xml castToCheckMenuItem "wrapMenuItem"
  curveedgesmenuitem <- xmlGetWidget xml castToCheckMenuItem "curveEdgesMenuItem"
  nooverlapmenuitem <- xmlGetWidget xml castToCheckMenuItem "noOverlapMenuItem"

  colorLevel0 <- xmlGetWidget xml castToMenuItem "colorLevel0"
  colorLevel1 <- xmlGetWidget xml castToMenuItem "colorLevel1"
  colorLevel2 <- xmlGetWidget xml castToMenuItem "colorLevel2"
  colorLevel3 <- xmlGetWidget xml castToMenuItem "colorLevel3"

  edgecolor <- xmlGetWidget xml castToMenuItem "edgeColor"

  errordialog <- xmlGetWidget xml castToMessageDialog "errorDialog"
  aboutdialog <- xmlGetWidget xml castToAboutDialog "aboutdialog"

  aboutDialogSetVersion aboutdialog showAbout

  filechooserdialog <- xmlGetWidget xml castToFileChooserDialog "filechooserdialog"

  lensdialog <- xmlGetWidget xml castToDialog "lensdialog"
  lensscale <- xmlGetWidget xml castToHScale "lensscale"

  editLensClose <- xmlGetWidget xml castToButton "editLensClose"

  colorselectiondialog <- xmlGetWidget xml castToColorSelectionDialog "colorselectiondialog"

  ffLsr <- fileFilterNew
  fileFilterAddPattern ffLsr "*.lsr"
  fileFilterSetName ffLsr "Lobster files"
  fileChooserAddFilter filechooserdialog ffLsr

  ffAll <- fileFilterNew
  fileFilterAddPattern ffAll "*"
  fileFilterSetName ffAll "All files"
  fileChooserAddFilter filechooserdialog ffAll

  widgetShowAll win

  onDestroy win mainQuit
  onActivateLeaf quitMenuItem $ widgetDestroy win

  mv <- newMVar $ St
    { stCfg = cfg0
    , stVSt = initVSt cfg0
    , standardSize = (0,0)
    , mVGraph = Nothing
    , drawingArea = drawingarea
    , scrolledWindow = scrolledwindow
    , viewport = view
    , window = win
    , eventXY = (0,0)
    , popupMenu = popupmenu
    , fileChooserDialog = filechooserdialog
    , errorDialog = errordialog
    , colorSelectionDialog = colorselectiondialog
    , colorIndex = EdgeColor
    , lensDialog = lensdialog
    , lensScale = lensscale
    , stRender = return ()
    }

  onButtonPress drawingarea $ \e -> (handleErr mv $ buttonPress e) >> return (eventSent e)

  onActivateLeaf aboutMenuItem $ widgetShowAll aboutdialog

  onActivateLeaf helpMenuItem $ handleErr mv $ \_ -> do
    ec <- system $ "x-www-browser " ++ helpFn ++ " &"
    case ec of
      ExitSuccess -> return ()
      ExitFailure i -> error $ "unable to open browser:(exit code=" ++ show i ++ ")"

  onResponse aboutdialog $ \_ -> widgetHide aboutdialog

  onResponse errordialog $ \_ -> widgetHide errordialog

  onActivateLeaf openMenuItem $ widgetShowAll filechooserdialog

  onActivateLeaf editLens $ widgetShowAll lensdialog

  onActivateLeaf expandAllMenuItem $ handleErr mv $ expandCollapseAll True
  onActivateLeaf collapseAllMenuItem $ handleErr mv $ expandCollapseAll False
  onActivateLeaf expandOrCollapsePopupItem $ handleErr mv expandOrCollapse

  onActivateLeaf closeMenuItem $ handleErr mv $ openFiles []

  onActivateLeaf colorLevel0 $ handleErr mv $ selectColor $ LevelColor 0
  onActivateLeaf colorLevel1 $ handleErr mv $ selectColor $ LevelColor 1
  onActivateLeaf colorLevel2 $ handleErr mv $ selectColor $ LevelColor 2
  onActivateLeaf colorLevel3 $ handleErr mv $ selectColor $ LevelColor 3

  onActivateLeaf edgecolor $ handleErr mv $ selectColor EdgeColor

  onResponse filechooserdialog $ \r -> handleErr mv $ fileChosen r

  onFileActivated filechooserdialog $ handleErr mv $ fileChosen (ResponseUser 1)

  colorSelectionOk <- colorSelectionDialogGetOkButton colorselectiondialog
  colorSelectionCancel <- colorSelectionDialogGetCancelButton colorselectiondialog
  colorSelectionHelp <- colorSelectionDialogGetHelpButton colorselectiondialog

  onClicked colorSelectionOk $ handleErr mv setColor
  onClicked colorSelectionCancel $ widgetHide colorselectiondialog
  onClicked colorSelectionHelp $ return ()

  onClicked editLensClose $ widgetHide lensdialog
  afterRangeValueChanged lensscale $ handleErr mv updateLensSize

  onToggleLens standardlens False mv
  onToggleLens stretchlens True mv

  onToggle wrapmenuitem $ handleErr mv $ \_ ->
    checkMenuItemGetActive wrapmenuitem >>= setIsWrap mv

  onToggle curveedgesmenuitem $ handleErr mv $ \_ ->
    checkMenuItemGetActive curveedgesmenuitem >>= setIsCurveEdges mv

  onToggle nooverlapmenuitem $ handleErr mv $ \_ ->
    checkMenuItemGetActive nooverlapmenuitem >>= setIsNoOverlap mv

  onExpose drawingarea $ \_ -> handleErr mv redraw >> return True

  onKeyPress win $ \e -> handleErr mv (keyPress e) >> return True

  handleErr mv $ openFiles $ filePaths cfg0

  mainGUI

updateLensSize :: MVar St -> IO ()
updateLensSize mv = do
  px <- getLensSize mv
  lt <- getLensType mv
  case lt of
    StandardLens -> centerLens mv
    StretchLens pos _ -> setLensType mv $ StretchLens pos px

keyPress :: Event -> MVar St -> IO ()
keyPress e mv = case eventKeyName e of
  "Up" -> moveLens 0 (-moveLensPx) mv
  "Down" -> moveLens 0 moveLensPx mv
  "Left" -> moveLens (-moveLensPx) 0 mv
  "Right" -> moveLens moveLensPx 0 mv
  _ -> return ()

moveLensPx :: Px
moveLensPx = 100

moveLens :: Px -> Px -> MVar St -> IO ()
moveLens dx dy mv = do
  lt <- getLensType mv
  case lt of
    StandardLens -> return ()
    StretchLens pos0@(x0,y0) px -> do
      (w,h) <- getStandardSize mv
      let (x1,y1) = (x0 + dx, y0 + dy)
      let pos = (min (w - px) (max 0 x1), min (h - px) (max 0 y1))
      when (pos /= pos0) $ do
        setLensType mv $ StretchLens pos px

handleErr :: MVar St -> (MVar St -> IO ()) -> IO ()
handleErr mv m = CE.handle h (m mv)
  where
  h :: CE.SomeException -> IO ()
  h e = showErrorDialog mv $ show e

showErrorDialog :: MVar St -> String -> IO ()
showErrorDialog mv s = do
  dia <- getErrorDialog mv
  set dia [messageDialogText := Just s]
  widgetShowAll dia

data ColorIndex
  = LevelColor Int
  | EdgeColor
  deriving Show

selectColor :: ColorIndex -> MVar St -> IO ()
selectColor ci mv = do
  modifySt mv $ \st -> st{ colorIndex = ci }
  dia <- getColorSelectionDialog mv
  rgba <- getColor mv
  colorSelectionDialogGetColor dia >>= \c ->
    colorSelectionSetCurrentColor c (toColor rgba)
  widgetShowAll dia

toColor :: RGBA -> Color
toColor (r,g,b,_) = Color (toCol r) (toCol g) (toCol b)
  where
  toCol x = round $ x*65535

fromColor :: Color -> RGBA
fromColor (Color r g b) = (toCol r, toCol g, toCol b, transparency)
  where
  toCol x = fromIntegral x / 65535

setColor :: MVar St -> IO ()
setColor mv = do
  ci <- getColorIndex mv
  dia <- getColorSelectionDialog mv
  col <- liftM fromColor (colorSelectionDialogGetColor dia >>= colorSelectionGetCurrentColor)
  case ci of
    LevelColor i -> modifyCfg mv $ \cfg -> cfg{ levelColors = levelColors cfg // [(i,col)] }
    EdgeColor -> modifyCfg mv $ \cfg -> cfg{ edgeColor = col }
  widgetHide dia
  setRenderAndVSt mv

showHotSpots :: Bool
showHotSpots = False

redraw :: MVar St -> IO ()
redraw mv = do
  r <- getRender mv
  vst <- getVSt mv
  renderInDrawWindow mv $ do
    r
    when showHotSpots $ do
      setSourceRGBA 1 0 0 1
      sequence_ [ rectangle x1 y1 (x2 - x1) (y2 - y1) | ((x1,y1),(x2,y2)) <- M.elems $ graphTbl vst ]
      stroke

fileChosen :: ResponseId -> MVar St -> IO ()
fileChosen r mv = do
  dia <- getFileChooserDialog mv
  widgetHide dia
  case r of
    ResponseUser 1 -> fileChooserGetFilename dia >>= \m -> openFiles (maybeToList m) mv
    _ -> return ()

onToggleLens :: (CheckMenuItemClass i) => i -> Bool -> MVar St -> IO (ConnectId i)
onToggleLens a isStretch mv = onToggle a $ handleErr mv $ \_ -> do
  r <- checkMenuItemGetActive a
  when r $ if isStretch
    then centerLens mv
    else setLensType mv StandardLens

centerLens :: MVar St -> IO ()
centerLens mv = do
  (w,h) <- getStandardSize mv
  px <- getLensSize mv
  setLensType mv $ StretchLens (w/2 - px/2, h/2 - px/2) px

getLensSize :: MVar St -> IO Double
getLensSize mv = do
  scl <- liftM lensScale $ getSt mv
  pct <- rangeGetValue scl
  (w,h) <- getStandardSize mv
  return $ pct*(max w h)/100

expandOrCollapse :: MVar St -> IO ()
expandOrCollapse mv = do
  mg <- getMVGraph mv
  case mg of
    Nothing -> return ()
    Just g0 -> do
      vst <- getVSt mv
      pos <- getEventXY mv
      let mgid = lookupCollision pos $ graphTbl vst
      case mgid of
        [] -> return ()
        (gid:_) -> do
          let
            toggle g = if graphId g == gid
                         then g{ gExpanded = not $ gExpanded g }
                         else g
            g1 = fmapVGraph toggle g0
          when (g1 /= g0) $ do
            setVGraph mv $ Just g1

expandCollapseAll :: Bool -> MVar St -> IO ()
expandCollapseAll isExpand mv = do
  mg <- getMVGraph mv
  case mg of
    Nothing -> return ()
    Just g0 -> setVGraph mv $ Just $ fmapVGraph (\g -> g{ gExpanded = isExpand }) g0

setVGraph :: MVar St -> Maybe VGraph -> IO ()
setVGraph mv mg = do
  modifySt mv $ \st -> st{ mVGraph = mg }
  setRenderAndVSt mv

buttonPress :: Event -> MVar St -> IO ()
buttonPress e mv = case eventButton e of
  RightButton -> do
    mg <- getMVGraph mv
    case mg of
      Nothing -> return ()
      Just _ -> do
        modifySt mv $ \st -> st{ eventXY = (eventX e, eventY e) }
        popupmenu <- getPopupMenu mv
        menuPopup popupmenu Nothing
  _ -> return ()

renderInDrawWindow :: MVar St -> Render a -> IO a
renderInDrawWindow mv r = do
  dw <- getDrawWindow mv
  renderWithDrawable dw r

openFiles :: [FilePath] -> MVar St -> IO ()
openFiles fns mv = do
  modifyCfg mv $ \cfg -> cfg{ filePaths = fns }
  win <- getWindow mv
  windowSetTitle win $ "LViz " ++ if null fns then "" else head fns
  if null fns
    then setVGraph mv Nothing
    else do
      g <- loadFiles mv fns
      setVGraph mv $ Just g

loadFiles :: MVar St -> [FilePath] -> IO VGraph
loadFiles mv fns = do
  opts <- getOptions mv
  (errs, Policy.Domain d) <- parseAndInterpretPolicyFiles opts fns
  when (not $ null errs) $ do
    showErrorDialog mv $ unlines errs
  g <- toVGraph [] d
  return g

getSt :: MVar a -> IO a
getSt = readMVar

getCfg :: MVar St -> IO Cfg
getCfg = liftM stCfg . getSt

getVSt :: MVar St -> IO VSt
getVSt = liftM stVSt . getSt

modifySt :: MVar a -> (a -> a) -> IO ()
modifySt mv f = modifyMVar_ mv $ \st -> return $ f st

modifyCfg :: MVar St -> (Cfg -> Cfg) -> IO ()
modifyCfg mv f = modifySt mv $ \st -> st{ stCfg = f $ stCfg st }

getDrawingArea :: MVar St -> IO DrawingArea
getDrawingArea = liftM drawingArea . getSt

getWindow :: MVar St -> IO Window
getWindow = liftM window . getSt

getPopupMenu :: MVar St -> IO Menu
getPopupMenu = liftM popupMenu . getSt

getEventXY :: MVar St -> IO PosPx
getEventXY = liftM eventXY . getSt

getMVGraph :: MVar St -> IO (Maybe VGraph)
getMVGraph = liftM mVGraph . getSt

getFileChooserDialog :: MVar St -> IO FileChooserDialog
getFileChooserDialog = liftM fileChooserDialog . getSt

getErrorDialog :: MVar St -> IO MessageDialog
getErrorDialog = liftM errorDialog . getSt

getColorSelectionDialog :: MVar St -> IO ColorSelectionDialog
getColorSelectionDialog = liftM colorSelectionDialog . getSt

getColorIndex :: MVar St -> IO ColorIndex
getColorIndex = liftM colorIndex . getSt

getLevelColors :: MVar St -> IO (Array Int RGBA)
getLevelColors = liftM levelColors . getCfg

getEdgeColor :: MVar St -> IO RGBA
getEdgeColor = liftM edgeColor . getCfg

getOptions :: MVar St -> IO Options
getOptions = liftM options . getCfg

getColor :: MVar St -> IO RGBA
getColor mv = do
  ci <- getColorIndex mv
  case ci of
    LevelColor i -> do
      liftM (\a -> a ! i) (getLevelColors mv)
    EdgeColor -> getEdgeColor mv

getDrawWindow :: MVar St -> IO DrawWindow
getDrawWindow mv = do
  da <- getDrawingArea mv
  widgetGetDrawWindow da

getStandardSize :: MVar St -> IO SizePx
getStandardSize = liftM standardSize . getSt

getRender :: MVar St -> IO (Render ())
getRender = liftM stRender . getSt

setStandardSize :: MVar St -> SizePx -> IO ()
setStandardSize mv a = modifySt mv $ \st -> st{ standardSize = a }

getLensType :: MVar St -> IO LensType
getLensType = liftM lensType . getCfg

setLensType :: MVar St -> LensType -> IO ()
setLensType mv a = do
  modifyCfg mv $ \cfg -> cfg{ lensType = a }
  setRenderAndVSt mv

setIsWrap :: MVar St -> Bool -> IO ()
setIsWrap mv a = do
  modifyCfg mv $ \cfg -> cfg{ isWrap = a }
  setRenderAndVSt mv

setIsCurveEdges :: MVar St -> Bool -> IO ()
setIsCurveEdges mv a = do
  modifyCfg mv $ \cfg -> cfg{ isCurveEdges = a }
  setRenderAndVSt mv

setRenderAndVSt :: MVar St -> IO ()
setRenderAndVSt mv = do
  mg <- getMVGraph mv
  cfg <- getCfg mv
  (r,vst) <- case mg of
    Just g -> do
      let ((sz0,_),vst0) = runFigVGraph (initVSt cfg) g
      setStandardSize mv sz0
      let (((w,h),r),vst) = runFigVGraph vst0 g
      da <- getDrawingArea mv
      widgetSetSizeRequest da (ceiling w) (ceiling h)
      return (r,vst)
    Nothing -> return (return (), initVSt cfg)
  modifySt mv $ \st -> st{ stRender = r, stVSt = vst }
  invalidateDrawWindow mv

setIsNoOverlap :: MVar St -> Bool -> IO ()
setIsNoOverlap mv a = do
  modifyCfg mv $ \cfg -> cfg{ isNoOverlap = a }
  setRenderAndVSt mv

invalidateDrawWindow :: MVar St -> IO ()
invalidateDrawWindow mv = do
  dw <- getDrawWindow mv
  (w,h) <- drawableGetSize dw
  drawWindowInvalidateRect dw (Rectangle 0 0 w h) False
