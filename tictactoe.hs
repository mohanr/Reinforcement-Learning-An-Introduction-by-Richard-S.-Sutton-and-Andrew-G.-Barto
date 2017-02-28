module RL where
import Control.Monad.State
import qualified Data.Map as Map
import Control.Applicative
import Graphics.Gloss

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
data BoardState = BoardState { xloc :: [Int],
                               oloc :: [Int],
                               index :: Int
                             }  deriving (Show)

translationaccumulator :: Float -> Float -> [Int] -> [Float] -> [Picture] -> [Picture]
translationaccumulator _ _ _ [] ys = reverse ys
translationaccumulator _ _ []  _ ys = reverse ys
translationaccumulator x1 y (x:xs1) xs  ys = translationaccumulator (x1 + 90) (y + 90) xs1 xs ( (translate 0 (xs !! (x - 1)) $
                                                                         rotate 45 $ pictures [ rectangleWire x1 y,rectangleWire y x1]) :ys) 

drawBoard :: BoardState -> Picture
drawBoard (BoardState xloc oloc index)=
  Pictures $ [ translate x y $ rectangleWire 90 90| x<-[0,90..180], y<-[0,90..180] ] ++ [drawx]

drawx :: Picture
drawx = color black $ rotate 45 $
        pictures [rectangleWire 90 0, rectangleWire 0 90]

drawo :: Picture
drawo = color black $ thickCircle 35 2

main =  do print (runState getrow fun)
           let x = (runState getrow fun)
           let y = (runState getcolumn fun)
           print (getboardsize)
           display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState [1,2,3] [4,5,6] 1))
           return ()
 
