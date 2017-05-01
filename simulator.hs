module Simulator where
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import Graphics.Matplotlib
import Control.Monad.State
import qualified Data.Map as Map
import Text.Printf
import System.Random
import Data.Array
import Data.Foldable
import Data.List
import Control.Monad.ST
import Control.Monad
import Data.Array.Unboxed
import Data.Array.ST

karmbandit = 10 

alpha = 0.1 

runs =  2000 

iterations = 3000 

-- Unused Get matrix of uniformly distributed values
uniformrandmatrix :: Int -> Int ->  IO (Matrix Double)
uniformrandmatrix r c = do
  seed <- randomIO
  return (reshape c $ randomVector seed Uniform (r * c))

-- Get Vector of uniformly distributed values
uniformrandvector :: Int ->  IO (Vector Double)
uniformrandvector r  = do
  seed <- randomIO
  return  $ randomVector seed Uniform r

converttooneszeros :: (Vector Double) -> IO (Vector Double)
converttooneszeros m = do
  return $ step (m - 0.1)

randomlist :: Int -> Double -> Int -> Double-> IO [Double]
randomlist a a1 b b1 = getStdGen >>= return . Data.Foldable.toList .Data.Array.listArray(a,b) . randomRs (a1,b1)

--Unused
randommatrix ::  IO (Matrix Double)
randommatrix = do
    r  <- (randomlist 0 0 9 9)
    return $ (row r)
   
randomvector ::  Int -> Double -> IO (Vector Double)
randomvector runsi runsd = do
  r  <- (randomlist 1 1 runsi runsd)
  return $ fromList r

subtractone :: IO (Vector Double) -> IO (Vector Double)
subtractone v = do
  noniov <- v
  let xs = Numeric.LinearAlgebra.toList ( noniov ) in
    return $ fromList  [  1 - x | x <- xs]

matrixmean :: IO(Vector Double) -> IO Int
matrixmean mat = do
  m <- mat
  let ones = Data.List.length (Numeric.LinearAlgebra.find (==1.0) m) in
    let s = size $ m in
        if ones == 0 then return $ 0 else return $ (ones `div` s)

maxindexes :: Matrix Double -> IO (Vector Double)
maxindexes m = do
  let idxs = map maxIndex . toRows $ m in
    return $ fromList (map fromIntegral idxs)

mutateandcopy k v value = runST $ do
  w <- thawVector v
  writeVector w k value
  v0 <- freezeVector w
  return v0

runsimulations :: Double -> IO Int -- IO(Vector Double)
runsimulations  alpha = simulate 2000 (fromList (take iterations (repeat 0))) (fromList (take iterations (repeat 0)))  
                        iterations karmbandit (matrix karmbandit (map fromIntegral [1..(runs * 10)])* 0.0) (matrix karmbandit (map fromIntegral [1..(runs*10)]) * 0.0 ) (matrix karmbandit (map fromIntegral [1..(runs * 10)])* 0.0)
                           where
                             simulate :: Int -> Vector Int -> Vector Int -> Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> IO Int 
                             simulate x recordsaver optimalsaver iter k q n bandit =
                               case () of _
                                            | x >= iter -> do
                                                m <- uniformrandvector runs
                                                s <- subtractone(converttooneszeros m) 
                                                -- print $ size $ s
                                                -- q1 <- (maxindexes  q)
                                                -- print $ size $ q1
                                                let a = (liftM2 (+) (liftM2  (*) (converttooneszeros m) (randomvector runs (fromIntegral runs)))
                                                         (liftM2  (*) (subtractone(converttooneszeros m)) (maxindexes  q))) in
                                                  let opt = maxindexes bandit in
                                                    do
                                                      matrixmean opt
                                            | x < iter ->  simulate (x + 1 ) recordsaver optimalsaver iter k q n bandit
main = do
  m <- runsimulations 0
  print m
  let n = (mutateandcopy 0 (fromList [1,2]:: Vector Int)  2) in
    print n
  return ()
