module Simulator where
import Numeric.LinearAlgebra
import Graphics.Matplotlib
import Control.Monad.State
import qualified Data.Map as Map
import Text.Printf

fun :: Map.Map String Double
fun = Map.empty


store :: String -> Double -> State (Map.Map String Double) ()
store x value = do
  fun <- get
  put (Map.insert x value fun)

retrieve :: String -> State (Map.Map String Double) (Maybe (Double))
retrieve value = do
  fun <- get
  return (Map.lookup  value fun) 


karmbandit = do {store "karmbandit" 10; retrieve "karmbandit"} 
alpha = do {store "alpha" 0.1; retrieve "alpha"} 
epsilon = do {store "epsilon" 0.1; retrieve "epsilon"} 
runs = do {store "runs" 2000; retrieve "runs"} 
iterations = do {store "iterations" 3000; retrieve "iterations"} 

runsimulations :: Double -> Matrix R
runsimulations  alpha = let recordsaver = matrix 1 (runState iterations fun ) * 0 in
                          let optimalsaver = matrix 1 (runState iterations fun ) * 0 in
                            let k = (runState karmbandit fun ) in
                              let bandit = matrix (runState iterations fun ) [k] in
                                let Q = matrix (runState iterations fun ) [k] in
                                  let N = matrix (runState runs fun ) [k] in
                                    simulate 0 Q N
                                     where
                                       simulate :: Int -> Int -> Int -> Matrix R
                                       simulate n Q N=
                                         case () of _
                                                      | n < Q -> do
                                                          randomR (1,N) (mkStdGen 66)
                                                          matrix 2 [1..6]
                                                      | n >= Q ->  simulate (x + 1 ) Q N

main = do
  print  (runState karmbandit fun)
