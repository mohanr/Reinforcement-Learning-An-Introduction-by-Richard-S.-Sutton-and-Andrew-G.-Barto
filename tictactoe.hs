module RL where
import Control.Monad.State
import qualified Data.Map as Map

fun :: Map.Map String Int
fun = Map.empty

f :: String -> Int-> State (Map.Map String Int) ()
f row value = do
  fun <- get
  put (Map.insert row value fun)

g :: String -> State (Map.Map String Int) (Maybe (Int))
g roworcolumn = do
  fun <- get
  return (Map.lookup roworcolumn fun) 

main =  print (runState (do {f "row" 1; g "row"}) fun) 
