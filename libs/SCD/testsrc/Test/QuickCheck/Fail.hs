module Test.QuickCheck.Fail(module Test.QuickCheck, check, test, quickCheck,
  verboseCheck) where

import Test.QuickCheck
import System.Random(newStdGen, StdGen, split)
import Data.List(sort, group, intersperse)

-- quick :: Config
-- quick = Config
--   { configMaxTest = 100
--   , configMaxFail = 1000
--   , configSize    = (+ 3) . (`div` 2)
--   , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
--   }

-- verbose :: Config
-- verbose = quick
--   { configEvery = \n args -> show n ++ ":\n" ++ unlines args
--   }

-- test, quickCheck, verboseCheck :: Testable a => a -> IO ()
-- test         = check quick
-- quickCheck   = check quick
-- verboseCheck = check verbose

-- check :: Testable a => Config -> a -> IO ()
-- check config a =
--   do rnd <- newStdGen
--      tests config (evaluate a) rnd 0 0 []

-- tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO () 
-- tests config gen rnd0 ntest nfail stamps
--   | ntest == configMaxTest config = do done "OK, passed" ntest stamps
--   | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
--   | otherwise               =
--       do putStr (configEvery config ntest (arguments result))
--          case ok result of
--            Nothing    ->
--              tests config gen rnd1 ntest (nfail+1) stamps
--            Just True  ->
--              tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
--            Just False -> 
--              do putStr ( "Falsifiable, after "
--                       ++ show ntest
--                       ++ " tests:\n"
--                       ++ unlines (arguments result)
--                        )
--                 fail "QuickCheck test failed"
--      where
--       result      = generate (configSize config ntest) rnd2 gen
--       (rnd1,rnd2) = split rnd0

-- done :: String -> Int -> [[String]] -> IO ()
-- done mesg ntest stamps =
--   do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
--  where
--   table = display
--         . map entry
--         . reverse
--         . sort
--         . map pairLength
--         . group
--         . sort
--         . filter (not . null)
--         $ stamps

--   display []  = ".\n"
--   display [x] = " (" ++ x ++ ").\n"
--   display xs  = ".\n" ++ unlines (map (++ ".") xs)

--   pairLength xss@(xs:_) = (length xss, xs)
--   entry (n, xs)         = percentage n ntest
--                        ++ " "
--                        ++ concat (intersperse ", " xs)

--   percentage n m        = show ((100 * n) `div` m) ++ "%"
