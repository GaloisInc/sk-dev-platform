module QC where

import Random
import Data.Char
import Data.Maybe
import Control.Monad.State

oneof :: [GenT st a] -> GenT st a
oneof [] = error "empty oneof"
oneof gs = do
  i <- choose 1 (length gs)
  gs !! (i-1)

elements :: [a] -> GenT st a
elements [] = error "empty elements"
elements gs = oneof $ map return gs

genListLen :: Int -> GenT st a -> GenT st [a]
genListLen n g = sequence $ replicate n g

genListMinMaxLen :: Int -> Int -> GenT st a -> GenT st [a]
genListMinMaxLen m n g = do
  x <- choose m n
  genListLen x g  

genListMaxLen :: Int -> GenT st a -> GenT st [a]
genListMaxLen = genListMinMaxLen 0

pick1 :: [a] -> GenT st (Maybe (a,[a]))
pick1 [] = return Nothing
pick1 xs = do
  i <- choose 0 (length xs - 1)
  let (xs1,x:xs2) = splitAt i xs
  return $ Just (x,xs1 ++ xs2)

pickN :: Int -> [a] -> GenT st (Maybe ([a],[a]))
pickN 0 xs = return $ Just ([],xs)
pickN n xs = do
  m0 <- pick1 xs
  case m0 of
    Nothing -> return Nothing
    Just (x,xs1) -> do
      m1 <- pickN (pred n) xs1
      case m1 of
        Nothing -> return Nothing
        Just (xs2,xs3) -> return $ Just (x:xs2,xs3)

pickMinMaxN :: Int -> Int -> [a] -> GenT st (Maybe ([a],[a]))
pickMinMaxN n0 n1 xs = do
  x <- choose n0 (min n1 $ length xs)
  pickN x xs

-- pickMaxN_ :: Int -> [a] -> GenT st [a]
-- pickMaxN_ n = liftM fst . pickMaxN n

-- pick :: [a] -> GenT st ([a],[a])
-- pick xs = pickMaxN (length xs) xs

-- pick_ :: [a] -> GenT st [a]
-- pick_ = liftM fst . pick

choose :: Int -> Int -> GenT st Int
choose x y = do
  g <- getQCStdGen
  let (i,g1) = randomR (x,y) g
  setQCStdGen g1
  return i

getQCStdGen :: GenT st StdGen
getQCStdGen = liftM qcStdGen getQCSt

setQCStdGen :: StdGen -> GenT st ()
setQCStdGen g = modifyQCSt $ \qc -> qc{ qcStdGen = g }

modifyQCSt :: (QCSt -> QCSt) -> GenT st ()
modifyQCSt f = modify $ \(st,qc) -> (st,f qc)

getQCSt :: GenT st QCSt
getQCSt = liftM snd get

modifySt f = modify $ \(st,g) -> (f st,g)

getSt :: GenT st st
getSt = liftM fst get

setSt :: st -> GenT st ()
setSt st = modifySt $ \ _ -> st

data QCSt = QCSt
  { qcStdGen :: StdGen
  , qcSize :: Int
  }

getQCSize :: GenT st Int
getQCSize = liftM qcSize getQCSt

modifyQCSize :: (Int -> Int) -> GenT st ()
modifyQCSize f = modifyQCSt $ \qc -> qc{ qcSize = f (qcSize qc) }

setQCSize :: Int -> GenT st ()
setQCSize i = modifyQCSize $ \_ -> i

type GenT st a =  StateT (st,QCSt) IO a

evalGenT :: st -> GenT st a -> IO a
evalGenT st m = do
  g <- getStdGen
  evalStateT m (st,QCSt g 0)

genInteger :: GenT st Integer
genInteger = liftM fromIntegral genInt

genInt :: GenT st Int
genInt = elements [ minBound, -1, 0, 1, maxBound ]

genNat :: GenT st Int
genNat = elements [ 0, 1, maxBound ]

genString :: Int -> GenT st String
genString x = genListMaxLen x genChar

genChar :: GenT st Char
genChar = elements ['a' .. 'z'] -- fixme: put back in liftM chr $ choose 0 255

genBool :: GenT st Bool
genBool = elements [ False, True ]
