module ReinforcementLearning where
import Control.Monad.State
import qualified Data.Map as Map
import Control.Applicative
import Graphics.Gloss
import Data.Array.IO
import Control.Monad.Reader
import System.Random
import Data.List
import Control.Exception
import System.IO.Error 
import Text.Printf
import Debug.Trace

fun :: Map.Map String Int
fun = Map.empty


store :: String -> Int-> State (Map.Map String Int) ()
store x value = do
  fun <- get
  put (Map.insert x value fun)

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

magicsquare :: [Int]
magicsquare = [2,9,4,7,5,4,7,5,4] 

data BoardState = BoardState { xloc :: [Int],
                               oloc :: [Int],
                               index :: Int
                             }  deriving (Show)

translationaccumulator ::   [Int] -> [Int] -> [(Float,Float)] -> [Picture] -> [Picture]
translationaccumulator  [] _ _ ys = reverse ys
translationaccumulator  _ []  _ ys = reverse ys
translationaccumulator  (head1:xs1) (head:xs) angle  ys = let (a,b) = (angle !!(head - 1)) in
                                                            let (c,d) = (angle  !!(head1 - 1)) in
                                                              translationaccumulator xs1 xs angle ( ((translate a b) $
                                                                                                 drawx ) : ((translate c d) $
                                                                                                 drawo ):ys)

drawBoard :: BoardState -> Picture
drawBoard (BoardState xloc oloc index)=
  Pictures $ [ translate x y $ rectangleWire 90 90| x<-[0,90..180], y<-[0,90..180] ] ++ (translationaccumulator xloc oloc [(0,180),(90,180),(180,180),(0,90),(90,90),(180,90),(0,0),(90,0),(180,0)] [])

drawx :: Picture
drawx = color green $ rotate 45 $
        pictures [rectangleWire 1 45, rectangleWire  45 1] 

drawo :: Picture
drawo = color rose $ thickCircle 25 2

powersof2  :: [Int]  
powersof2  =  [ 2 ^ i | i <- [0..9]]


createarray :: IO ( IOArray Int Double)
createarray =  do {
                       arr <- newArray (0,512*512) (-1);
                       return arr
                  }

stateindex :: [Int] -> [Int] -> Int  
stateindex xloc oloc =  let powers = powersof2 in
                          ((foldl (+) 0 [  ( powers !!n) | n <- [0..(length xloc - 1)]]) +
                          ( 512 * foldl (+) 0 [  ( powers !!n) | n <- [0..(length oloc - 1)]]))

type ArrayAccess = ReaderT  (IOArray Int Double)  IO 
type ArrayWriteAccess = ReaderT  (IOArray Int Double)  IO() 

readvalue ::  Int -> ArrayAccess  Double  
readvalue x    = do 
  a <- ask
  b <- liftIO( readArray a x);    
  return b

writevalue ::  Int -> Double -> ArrayWriteAccess   
writevalue x y   = do 
  a <- ask
  liftIO( writeArray a x y)    

-- Test array accesses
readfromarray = do { a <- createarray; liftIO (runReaderT (readvalue 1) a) }
writetoarray = do { a <- createarray; liftIO (runReaderT (writevalue 1 2) a) }


showstate :: BoardState -> IO ()
showstate (BoardState xloc oloc index) = display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState xloc oloc index) )

data Player = X | O deriving Show
isX :: Player -> Bool
isX X = True
isX O = False 

type StateValue sv = StateT BoardState IO sv

append :: Int -> [Int] -> [Int]
append elem l = l ++ [elem]

readthevalue :: ( IOArray Int Double) -> Int -> IO Double
readthevalue a index =  liftIO (runReaderT (readvalue index ) a) 

writethevalue :: ( IOArray Int Double) -> Int -> Double -> IO ()
writethevalue a index value =  liftIO (runReaderT (writevalue index value) a)  

nextstate :: Player -> BoardState -> Int -> BoardState
-- nextstate  player (BoardState xloc oloc index) move= traceShowId $  BoardState newx newo newindex where
nextstate  player (BoardState xloc oloc index) move=  BoardState newx newo newindex where
  newx = if isX player then (append move xloc) else xloc
  newo = if isX player then (append move oloc) else oloc
  newindex = stateindex newx newo

magicnumber :: [Int]-> Int
magicnumber l = sum $ ([magicsquare !! (x-1) | x <- l])


nextvalue :: Player -> Int -> ( IOArray Int Double) -> BoardState-> IO (BoardState,IOArray Int Double) 
nextvalue  player move a ( BoardState xloc oloc index) =  do
  x <- catch (readthevalue a index)(\(SomeException e) -> printf "Reading [%d} in Next value" index >> print e >> throwIO e)
  let newstate = (nextstate player ( BoardState xloc oloc index) move)
  printf "Old state index is [%d]" index
  printf "New state index is [%d]"  (ReinforcementLearning.index newstate)
  -- mapM_ (putStr . show) xloc
  -- mapM_ (putStr . show) oloc
  if (x == 0)
  then if ((magicnumber xloc ) == 15)
       then do
            (writethevalue a index 0)
            return (newstate,a)
       else if ((magicnumber oloc ) == 15)
            then do
                 (writethevalue a index 1)
                 return (newstate,a)
            else if ((length oloc )+(length xloc) == 9)
            then do
                 (writethevalue a index 0)
                 return (newstate,a)
            else return (newstate,a)
  else return (newstate,a)

--   Returns a list of unplayed locations
possiblemoves :: BoardState -> [Int]
possiblemoves (BoardState xloc oloc index) =
  let xs =  [1,2,3,4,5,6,7,8,9] in
    (xs \\ xloc) \\ oloc

debug :: IO Int -> IO Int
debug value = do
  x <- value
  printf "Random move to [%d]\n" x
  return x

--   "Returns one of the unplayed locations, selected at random"
randommove ::  BoardState -> IO Int
randommove state = 
  let possibles = possiblemoves state in
    case possibles of
      -- p -> debug $ fmap (p !! ) $ randomRIO(0, length p - 1)
      p ->  fmap (p !! ) $ randomRIO(0, length p - 1)
              
update :: ( IOArray Int Double) -> BoardState -> BoardState -> IO ( IOArray Int Double)
update a state newstate = do
  valueofstate <- readthevalue a (ReinforcementLearning.index state)
  valueofnewstate <- readthevalue a (ReinforcementLearning.index newstate)

  let finalvalue = valueofstate + ( 0.5 *  (valueofnewstate - valueofstate)) in
  --  This is the learning rule
    writethevalue a (ReinforcementLearning.index state) finalvalue
  return a

randombetween :: IO Double
randombetween = do
  r1 <-  randomRIO(0, 1.0)
  return r1


terminalstatep :: ( IOArray Int Double) -> Int -> IO Bool
terminalstatep a x = do
  y <-  catch ( readthevalue a x) (\(SomeException e) ->  print e >> printf "The index of value read is [%d]" x >> throwIO e)
  let result = (y == fromIntegral( round y))
  do {
    putStrLn (show y);
    putStrLn ( show result);
    return result
    }
  
greedymove :: ( IOArray Int Double) ->Player -> BoardState -> IO (Int,IOArray Int Double)
greedymove a player state = 
  let possibles = possiblemoves state in
    case possibles of
      p  -> let bestvalue = -1.0 in
              let bestmove = 0 in
                choosebestmove p bestvalue bestmove
                where
                  choosebestmove (x:xs) bestvalue bestmove = do
                    (nv,a) <- nextvalue player x a state
                    xvalue <-  catch (readthevalue a (ReinforcementLearning.index (nv)))(\(SomeException e) -> printf "Reading [%d} in greedy move" x >> print e >> throwIO e)
                    case compare bestvalue xvalue of
                      LT -> choosebestmove  xs bestvalue bestmove;
                      GT -> return (bestmove,a)

randomgreedy :: Double -> Int -> Int -> Int
randomgreedy r1 rm gm = if (r1 < 0.01)
                  then rm
                  else gm



gameplan :: ( IOArray Int Double) -> BoardState -> BoardState -> IO (IOArray Int Double,BoardState,Double) 
gameplan a state newstate = do 
  r1 <- randombetween;
  result <- (terminalstatep a (ReinforcementLearning.index newstate));
    case result of
      True -> do
        a <- update a state newstate
        valueofnewstate <- catch (readthevalue a (ReinforcementLearning.index newstate)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index newstate)]>> throwIO e)
        printf "Gameplan returns(True branch) %f " valueofnewstate
        return (a,state,valueofnewstate)
      False -> do
        rm <- randommove newstate
        (gm,a) <- greedymove a O newstate
        (nv,a) <- nextvalue O (randomgreedy r1 rm gm) a state
        unless (r1 < 0.01) $ do
          a <- (update a state nv)
          return ()
        result <- (terminalstatep a (ReinforcementLearning.index nv));
          valueofnewstate <-  catch (readthevalue a (ReinforcementLearning.index nv)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index nv)]>> throwIO e)
        if result
          then do
          printf "Gameplan returns(False branch) %f " valueofnewstate
          return (a,nv,valueofnewstate)
          else do
          r <- randommove state
          (nv1,a) <- nextvalue X r a state
          gameplan a newstate (nv1)
  

--   "Plays 1 game against the random player. Also learns and prints.
--    :X moves first and is random.  :O learns"
game :: BoardState  -> BoardState -> IOArray Int Double -> IO (IOArray Int Double,BoardState,Double) 
game state newstate a  = do
  initialvalue <- readthevalue  a 0
  printf "Gameplan with Initial value is %f \n" initialvalue
  gameplan a state newstate

playntimes :: Int -> IO ()
playntimes n = do a <- createarray;
                  writethevalue a 0 0.5
                  r <- (randommove (BoardState [0,0,0] [0,0,0] 0))
                  playtime a (BoardState [0,0,0] [0,0,0] 0) (nextvalue X r a (BoardState [0,0,0] [0,0,0] 0)) n 0 r
                    where
                      playtime :: IOArray Int Double -> BoardState -> IO (BoardState,IOArray Int Double) -> Int -> Double -> Int -> IO ()
                      playtime a s ns n acc r
                        | n == 0 = printf "\nPlayed 100 times %f  %f" acc (acc/100.0)
                        | n > 0 = do
                            (boardstate, _) <- ns 
                            (newa, state, result )<- game s  boardstate a; 
                            -- printf "Game returns %f\n" result
                            r1 <- randommove state
                            playtime newa state (nextvalue X  r1 a state) (n - 1) (acc + result) r1
  
main =  do print (runState getrow fun)

           ReinforcementLearning.playntimes 100
           display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState [1,2,3] [4,5,6] 1))
           return ()
