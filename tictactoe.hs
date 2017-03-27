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
import System.IO

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
powersof2  =  [ 2 ^ i | i <- [0..8]]


createarray :: IO ( IOArray Int Double)
createarray =  do {
                       arr <- newArray (0,512*512) 0;
                       return arr
                  }

addVal :: Int -> [Int] -> [Int]
addVal i [] = []
addVal i (x:xs) = x * 512: addVal i xs

stateindex :: [Int] -> [Int] -> Int
stateindex xloc oloc = sum (map (2^) xloc)
                       + sum [2^n | n <- (addVal 512 oloc)]

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

logs      ::  String -> IO ()
logs  message = withFile "D:/Git/game.log" AppendMode (\ fd -> hPrint fd message )

showstate :: BoardState -> IO ()
showstate (BoardState xloc oloc index) = display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState xloc oloc index) )

data Player = X | O deriving Show
isX :: Player -> Bool
isX X = True
isX O = False 

type StateValue sv = StateT BoardState IO sv

append :: Int -> [Int] -> [Int]
append elem l = if elem == 0 then l else l ++ [elem]

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
magicnumber l = sum $ ([magicsquare !! (x-1) | x <- l, x > 0])


nextvalue :: (String -> IO()) -> Player -> Int -> ( IOArray Int Double) -> BoardState-> IO (BoardState,IOArray Int Double) 
nextvalue log player move a ( BoardState xloc oloc index) =  do
  let newstate = (nextstate player ( BoardState xloc oloc index) move)
  x <- catch (readthevalue a (ReinforcementLearning.index newstate))(\(SomeException e) -> printf "Reading [%d} in Next value" index >> print e >> throwIO e)
  log $ printf "Move is [%d] Value from value table is %f" move x
  log $ (show player)
  log $ show (ReinforcementLearning.xloc newstate)
  log $ show (ReinforcementLearning.oloc newstate)
  log $ printf "Magic number is %d" (magicnumber  (ReinforcementLearning.xloc newstate))
  log $ printf "Magic number is %d" (magicnumber  (ReinforcementLearning.oloc newstate))
  if (x == 0.0)
  then if ((magicnumber xloc ) == 15)
       then do
            (writethevalue a (ReinforcementLearning.index newstate) 0)
            return (newstate,a)
       else if ((magicnumber oloc ) == 15)
            then do
                 (writethevalue a  (ReinforcementLearning.index newstate) 1)
                 return (newstate,a)
            else if ((length oloc )+(length xloc) == 9)
            then do
                 (writethevalue a  (ReinforcementLearning.index newstate) 0)
                 return (newstate,a)
            else do
                 (writethevalue a  (ReinforcementLearning.index newstate) 0.5)
                 return (newstate,a)
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
      [] -> return 0
      p -> debug $ fmap (p !! ) $ randomRIO(0, length p - 1)
      -- p ->  fmap (p !! ) $ randomRIO(0, length p - 1)
              
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
  y <-  catch ( readthevalue a x) (\(SomeException e) ->  print e >> printf "Read in terminalstep throws exception" >> throwIO e)
  let result = (y == fromIntegral( round y))
  do {
    -- putStrLn (show y);
    -- putStrLn ( show ( fromIntegral( round y)));
    -- return result
    return result
    }
  
greedymove :: ( IOArray Int Double) ->Player -> BoardState -> IO (Int,IOArray Int Double)
greedymove a player state = 
  let possibles = possiblemoves state in
    case possibles of
      [] -> return (0, a)
      p  -> let bestvalue = -1.0 in
              let bestmove = 0 in
                choosebestmove a p bestvalue bestmove
                where
                  choosebestmove arr [] bestvalue1 bestmove1 = return (0,a)
                  choosebestmove arr (x:xs) bestvalue1 bestmove1 = do
                    (nv,b) <- nextvalue logs player x arr state
                    xvalue <-  catch (readthevalue b (ReinforcementLearning.index (nv)))(\(SomeException e) -> printf "Reading [%d} in greedy move" x >> print e >> throwIO e)
                    case compare bestvalue1 xvalue of
                      LT -> choosebestmove b xs xvalue x;
                      GT -> return (bestmove1,b)
                      EQ -> return (bestmove1,b)
  
randomgreedy :: Double -> Int -> Int -> Int
randomgreedy r1 rm gm = if (r1 < 0.01)
                  then rm
                  else gm



gameplan :: (String -> IO()) ->( IOArray Int Double) -> BoardState -> BoardState -> IO (IOArray Int Double,BoardState,Double) 
gameplan log a state newstate = do 
  r1 <- randombetween;
  initialvalue <- readthevalue  a 0
  result <- (terminalstatep a (ReinforcementLearning.index newstate));
    case result of
      True -> do
        b <- update a state newstate
        valueofnewstate <- catch (readthevalue b (ReinforcementLearning.index newstate)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index newstate)]>> throwIO e)
        log $ printf "Gameplan returns(True branch) %f\n " valueofnewstate
        return (b,newstate,valueofnewstate)
      False -> do
        rm <- randommove newstate
        (gm,c) <- greedymove a O newstate
        (nv,d) <- nextvalue logs O (randomgreedy r1 rm gm) c newstate
        d' <- if r1 < 0.01 then return d else update d state nv
        result <- (terminalstatep d' (ReinforcementLearning.index nv));
        valueofnewstate <-  catch (readthevalue d' (ReinforcementLearning.index nv)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index nv)]>> throwIO e)
        if result
          then do
          log $ printf "Gameplan returns(False branch) %f\n " valueofnewstate
          return (d',nv,valueofnewstate)
          else do
          r <- randommove newstate
          (nv1,d') <- nextvalue logs X r d' newstate
          gameplan log d' newstate (nv1)
  

--   "Plays 1 game against the random player. Also learns and prints.
--    :X moves first and is random.  :O learns"
game ::  (String -> IO()) ->BoardState  -> BoardState -> IOArray Int Double -> IO (IOArray Int Double,BoardState,Double) 
game log state newstate a  = do
  (newa, state, result )<-  gameplan log a state newstate
  return (newa, state, result )
   
playntimes :: Int -> IO ()
playntimes n = do a <- createarray;
                  writethevalue a 0 0.5
                  r <- (randommove (BoardState [] [] 0))
                  playtime  (BoardState [] [] 0) (nextvalue logs X r a (BoardState [] [] 0)) n 0 r
                    where
                      playtime ::  BoardState -> IO (BoardState,IOArray Int Double) -> Int -> Double -> Int -> IO ()
                      playtime  s ns n acc r
                        | n == 0 = printf "\nPlayed 100 times %f  %f" acc (acc/100.0)
                        | n > 0 = do
                            (boardstate, b) <- ns 
                            (newa, state, result )<- game logs s  boardstate b; 
                            printf "Game returns %f\n" result
                            r1 <- randommove (BoardState [] [] 0)
                            -- playtime state (nextvalue logs X  r1 newa state) (n - 1) (acc + result) r1
                            playtime (BoardState [] [] 0) (nextvalue logs X  r1 newa (BoardState [] [] 0)) (n - 1) (acc + result) r1
  
main =  do print (magicnumber [0,0,0,8,9,7,2,5,1,6,3,4])
           ReinforcementLearning.playntimes 1 
           display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState [1,2,3] [4,5,6] 1))
           return ()
