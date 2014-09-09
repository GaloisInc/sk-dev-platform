{-# OPTIONS -Wall #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad
import Data.Char
import Data.List
import System
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process
import qualified Control.Exception as CE

data St = St
  { nPass :: Int
  , nFail :: Int
  , verbose :: Bool
  , noGldFile :: Bool
  } deriving (Show)

{-# NOINLINE g_st #-}
g_st :: MVar St
g_st = unsafePerformIO $ newMVar $ St 0 0 False False

data Flag 
  = Verbose
  | NoGldFile
  deriving (Show)
    
options :: [OptDescr Flag]
options =
 [ Option ['v'] ["verbose"] (NoArg Verbose) "print all results"
 , Option ['n'] ["no-gld-file"] (NoArg NoGldFile) "use lobster output instead of .gld file"
 ]
    
opts :: [String] -> ([Flag], [String])
opts argv = case getOpt Permute options argv of
  (o,n,[]) -> (o,n)
  (_,_,errs) -> error $ concat errs ++ usage

usage :: String
usage = usageInfo header options
  where header = "Usage: Test [OPTION...] files..."

setFlag :: Flag -> IO ()
setFlag fl = case fl of
  Verbose -> modifySt $ \st -> (st{ verbose = True },())
  NoGldFile -> modifySt $ \st -> (st{ noGldFile = True },())

main :: IO ()
main = do
  (flags,fns0) <- liftM opts getArgs
  mapM setFlag flags
  fns <- case fns0 of
    [] -> getLSRFiles
    _ -> return fns0
  mapM_ reportFile fns
  ec <- reportSummary
  exitWith ec

reportSummary :: IO ExitCode
reportSummary = do
  st <- getSt
  let tot = totalTests st
  let
    pct :: Double
    pct = 100 * (fromIntegral (nPass st)) / fromIntegral tot
  putStrLn $ "\nTotal Tests:" ++ show tot ++
             ":Pass:" ++ show (nPass st) ++ ":Fail:" ++ show (nFail st) ++
             ":Pct:" ++ show (round pct :: Int) ++ "%"
  if nPass st == tot
    then return ExitSuccess
    else return $ ExitFailure $ nFail st

reportFile :: FilePath -> IO ()
reportFile fn = do
  eab <- CE.try $ testFile fn
  case eab of
    Left e -> do
      n <- incFail
      showReport n $ "FAIL:" ++ show e
    Right () -> do
      n <- incPass
      isVerbose <- liftM verbose getSt
      if isVerbose
        then showReport n $ "PASS"
        else putStr "."
  where
  showReport testNum res =
    putStr $ "\nTest " ++ show testNum ++ ":" ++ fn ++ ":Result:" ++ res

modifySt :: (St -> (St,b)) -> IO b
modifySt f = modifyMVar g_st (return . f)

getSt :: IO St
getSt = modifySt $ \st -> (st,st)

incFail :: IO Int
incFail = modifySt $ \st -> let st1 = st{ nFail = succ (nFail st) } in (st1, totalTests st1)

incPass :: IO Int
incPass = modifySt $ \st -> let st1 = st{ nPass = succ (nPass st) } in (st1, totalTests st1)

totalTests :: St -> Int
totalTests st = nPass st + nFail st

testFile :: FilePath -> IO ()
testFile fn = do
  lobExitCode <- runLobster fn
  isNoGldFile <- liftM noGldFile getSt
  when (not isNoGldFile) $ do
    doesExist <- doesFileExist $ gldFile fn
    isFailIf (not doesExist) "no existing gold (.gld) file"
    isDiffGld <- diff (gldFile fn) (lobOutFile fn)
    isFailIf isDiffGld "lobster output (.out) differs from gold (.gld) file"
  if isErrFile fn
    then do
      isFailIf (lobExitCode == ExitSuccess) "expected error exit code from lobster"
    else do
      isFailIf (lobExitCode /= ExitSuccess) "expected success exit code from lobster"
      sl2csExitCode <- runSL2CS fn
      isFailIf (sl2csExitCode /= ExitSuccess) "expected success exit code from sl2cs"
      isDiffSL2CS <- diff (sl2csOutFile fn) (lobOutFile fn)
      isFailIf isDiffSL2CS "sl2cs output (.cso) differs from lobster output (.out)"

isFailIf :: Bool -> String -> IO ()
isFailIf isFail s | isFail = error s
                  | otherwise = return ()
    
runLobster :: FilePath -> IO ExitCode
runLobster fn = runAndWaitForProcess $ "../lobster.sh " ++ fn ++ " > " ++ lobOutFile fn

runSL2CS :: FilePath -> IO ExitCode
runSL2CS fn = runAndWaitForProcess $ "../sl2cs.sh " ++ fn ++ " > " ++ sl2csOutFile fn

runAndWaitForProcess :: String -> IO ExitCode
runAndWaitForProcess cmd = do
  ph <- runCommand cmd
  waitForProcess ph

diff :: FilePath -> FilePath -> IO Bool
diff fn1 fn2 = do
  ss1 <- liftM f $ readFile fn1
  ss2 <- liftM f $ readFile fn2
  return $ linesDiffer ss1 ss2
  where
  f = sort . filter (/= "") . map (dropWhile isSpace) . lines

linesDiffer :: [String] -> [String] -> Bool
linesDiffer [] [] = False
linesDiffer [] _ = True -- error "different number of lines"
linesDiffer _ [] = True -- error "different number of lines"
linesDiffer (x:xs) (y:ys) | x == y = linesDiffer xs ys
                        | otherwise = True -- error $ "lines differ:" ++ show (x,y)

getLSRFiles :: IO [FilePath]
getLSRFiles = do
  fns <- getDirectoryContents "."
  return $ sort $ filter isLSRFile fns

isErrFile :: FilePath -> Bool
isErrFile fn = take 3 fn == "err"

isLSRFile :: FilePath -> Bool
isLSRFile fn = takeExtension fn == ".lsr"

gldFile :: FilePath -> FilePath
gldFile fn = fn ++ ".gld"

lobOutFile :: FilePath -> FilePath
lobOutFile fn = fn ++ ".out"

sl2csOutFile :: FilePath -> FilePath
sl2csOutFile fn = fn ++ ".cso"
