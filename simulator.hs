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

main = do
  print  (runState karmbandit fun)
