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
import Numeric.LinearAlgebra.Data
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

converttooneszeros :: (Vector Double) ->  IO (Vector Double)
converttooneszeros m = do
  return $ step (m - 0.1)
converttooneszeros1 :: (Fractional e, Ord e, Container c e, Monad m) => c e -> m (c Z)
converttooneszeros1 = return . toZ. step . cmap (subtract 0.1)

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

listaverage :: (Fractional e )  => [e] -> e
listaverage l = let (sum,count) = foldr ( \lambda (s,c) -> (s+lambda,c+1)) (0,0) l in
                  sum/count

slicen :: Matrix Double -> Int -> Vector Double -> Vector Double 
slicen n range a1 =
  let newn = [  n `atIndex` (x,round y)| (x,y) <- zip  [0..range] (Numeric.LinearAlgebra.toList a1) ] in
    cmap (+1) (fromList newn :: Vector Double)

slicer :: Matrix Double -> Int -> Vector Double -> [Double] 
slicer bandit range a1 = [  bandit `atIndex` (x,round y)| (x,y) <- zip  [0..range] (Numeric.LinearAlgebra.toList a1) ] 

currentalpha  :: Bool -> Vector Double -> Vector Double  
currentalpha a v = if True then v else (cmap (1/) v)
  
runsimulations :: Double ->  IO (Vector Double )
runsimulations  alpha = simulate 3000 (fromList (take iterations (repeat 0.0))) (fromList (take iterations (repeat 0.0)))  
                        iterations karmbandit (matrix karmbandit (map fromIntegral [1..(runs * 10)])* 0.0) (matrix karmbandit (map fromIntegral [1..(runs*10)]) * 0.0 ) (matrix karmbandit (map fromIntegral [1..(runs * 10)])* 0.0)
                           where
                             simulate :: Int -> Vector Double -> Vector Double -> Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> IO (Vector Double ) 
                             simulate x rewardsaver optimalsaver iter k q n bandit =
                               case () of _
                                            | x >= iter -> do
                                                m <- uniformrandvector runs
                                                s <- subtractone(converttooneszeros m) 
                                                let a = (liftM2 (+) (liftM2  (*) (converttooneszeros m) (randomvector runs (fromIntegral runs)))
                                                         (liftM2  (*) (subtractone(converttooneszeros m)) (maxindexes  q))) in
                                                  let opt = maxindexes bandit in
                                                    do
                                                      mm <- matrixmean opt
                                                      a1 <- a
                                                      print $ size $ a1
                                                      print $ size $ bandit
                                                      let range = (size $ a1) in
                                                        let r = slicer bandit range a1 in
                                                          let rs =( Numeric.LinearAlgebra.accum  rewardsaver const [(x - 1,(listaverage r))]) in
                                                            let newn = slicen n range a1 in
                                                              let calpha = currentalpha True newn in
 
                                                              return $ Numeric.LinearAlgebra.accum  optimalsaver const [(x - 1,fromIntegral mm)]

                                                      
                                            | x < iter ->  simulate (x + 1 ) rewardsaver optimalsaver iter k q n bandit
main = do
  m <- runsimulations 0
  -- print m
  return ()
