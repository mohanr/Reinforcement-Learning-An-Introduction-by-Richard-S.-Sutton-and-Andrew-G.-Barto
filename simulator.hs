module Simulator where
import Numeric.LinearAlgebra
import Graphics.Matplotlib
import Control.Monad.State
import qualified Data.Map as Map
import Text.Printf
import System.Random



karmbandit = 10 

alpha = 0.1 

epsilon = 0.1 

runs =  2000 

iterations = 3000 

-- Get matrix of uniformly distributed values
uniformrand r c = do
  seed <- randomIO
  return (reshape c $ randomVector seed Uniform (r * c))

runsimulations :: Double -> IO(Matrix Double)
runsimulations  alpha = simulate 0 (matrix 1 [iterations] * 0) (matrix 1 [iterations] * 0)
                        iterations karmbandit (matrix runs [karmbandit] * 0) (matrix runs [karmbandit] * 0)
                        
                           where
                             simulate :: Double -> Matrix R -> Matrix R -> Double-> Double-> Matrix R -> Matrix R -> IO( Matrix Double) 
                             simulate x recordsaver optimalsaver iter k q n=
                               case () of _
                                            | x >= iter -> do
                                                uniformrand 10 10
                                            | x < iter ->  simulate (x + 1 ) recordsaver optimalsaver iter k q n
main = do
  m <- runsimulations 0
  print m 
