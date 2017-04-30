module Simulator where
import Numeric.LinearAlgebra
import Graphics.Matplotlib
import Control.Monad.State
import qualified Data.Map as Map
import Text.Printf
import System.Random
import Control.Monad
import Data.Array
import Data.Foldable


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

randomlist :: Double-> Double-> IO [Double]
randomlist a b = getStdGen >>= return . Data.Foldable.toList .listArray(0,9) . randomRs (a,b)

--Unused
randommatrix ::  IO (Matrix Double)
randommatrix = do
    r  <- (randomlist 0 9)
    return $ (row r)
   
randomvector ::  IO (Vector Double)
randomvector = do
    r  <- (randomlist 0 9)
    return $ fromList r

subtractone :: IO (Vector Double) -> IO (Vector Double)
subtractone v = do
  noniov <- v
  let xs = Numeric.LinearAlgebra.toList ( noniov ) in
    return $ fromList  [  1 - x | x <- xs]
    
maxindexes :: Matrix Double -> IO (Vector Double)
maxindexes m = do
  let idxs = map maxIndex . toRows $ m in
    return $ fromList (map fromIntegral idxs)
  
runsimulations :: Double -> IO(Vector Double)
runsimulations  alpha = simulate 2000 (matrix 1 [iterations] * 0) (matrix 1 [iterations] * 0)
                        iterations karmbandit (matrix runs [karmbandit] * 0) (matrix runs [karmbandit] * 0)
                        
                           where
                             simulate :: Double -> Matrix R -> Matrix R -> Double-> Double-> Matrix Double -> Matrix R -> IO( Vector Double) 
                             simulate x recordsaver optimalsaver iter k q n=
                               case () of _
                                            | x >= iter -> do
                                                m <- uniformrandvector 10
                                                s <- subtractone(converttooneszeros m) 
                                                liftM2 (+) (liftM2  (*) (converttooneszeros m) (randomvector))
                                                  (liftM2  (*) (subtractone(converttooneszeros m)) (maxindexes  q))
                                            | x < iter ->  simulate (x + 1 ) recordsaver optimalsaver iter k q n
main = do
  m <- runsimulations 0
  u <- uniformrandvector 10
  n <- converttooneszeros u
  n1 <- subtractone (converttooneszeros u)
  print n1
  return ()
