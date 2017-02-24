module RL where
import Control.Monad.State
import qualified Data.Map as Map
import Control.Applicative
fun :: Map.Map String Int
fun = Map.empty

store :: String -> Int-> State (Map.Map String Int) ()
store row value = do
  fun <- get
  put (Map.insert row value fun)
retrieve :: String -> State (Map.Map String Int) (Maybe (Int))
retrieve roworcolumn = do
  fun <- get
  return (Map.lookup roworcolumn fun) 

getrow = do {store "row" 1; retrieve "row"}  
getcolumn = do {store "column" 1; retrieve "column"}  
getboardsize = do   
           let x = (runState getrow fun) in
             let y = (runState getcolumn fun) in
                (Just (*) <*> (fst x)  <*>  (fst y) )

putmagicsquare = do { store "!" 2; store "2" 9;store "3" 4;
                      store "4" 7; store "5" 5;store "6" 4; 
                      store "6" 7; store "1" 5;store "8" 4; 
                    }
data BoardState = BoardState { first :: [Int],
                               second :: [Int],
                               index :: Int
                             }  deriving (Show)

main =  do print (runState getrow fun)
           let x = (runState getrow fun)
           let y = (runState getcolumn fun)
           print (getboardsize)
           return ()
 
 
