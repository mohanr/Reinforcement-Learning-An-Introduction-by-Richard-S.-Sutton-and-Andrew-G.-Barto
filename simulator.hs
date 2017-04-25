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

runsimulations :: Double -> Matrix R
runsimulations  alpha = simulate 0 (matrix 1 [iterations] * 0) (matrix 1 [iterations] * 0) iterations karmbandit
                           where
                             simulate :: Double -> Matrix R -> Matrix R -> Double-> Double-> Matrix R
                             simulate x q n iter k=
                               case () of _
                                            | x >= iter -> do
                                                -- randomR (1,k) (mkStdGen 66)
                                                matrix 2 [1..6]
                                            | x < iter ->  simulate (x + 1 ) q n iter k
main = print (runsimulations 0)
