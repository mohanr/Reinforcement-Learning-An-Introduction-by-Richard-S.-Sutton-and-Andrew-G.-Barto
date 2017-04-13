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
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

-- BEGIN Unused code --
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

-- END Unused code --
winningcombination :: [[Int]]
winningcombination = [[1,2,3],[4,5,6],[7,8,9],
                      [1,4,7],[2,5,8],[3,6,9],
                      [1,5,9],[3,5,8]]


checkallcombination ::  [Int] -> Bool
checkallcombination l = let wc = winningcombination in
                          loop wc
                            where
                              loop :: [[Int]] -> Bool
                              loop wc =
                                case wc of
                                  [] ->  False
                                  (x:xs) -> if containscombination x l then True else loop xs

containscombination :: [Int] -> [Int] -> Bool
containscombination xs xs1 =
  case xs of
    [] -> True 
    (x:xss) -> if (x `elem` xs1) then containscombination xss xs1 else False

createarray :: IO ( IOArray Int Double)
createarray =  do {
                       arr <- newArray (0,512*512) (-1.0);
                       return arr
                  }


stateindex :: [Int] -> [Int] -> Int
stateindex xloc oloc = sum [2^(n-1)| n <- xloc]
                       + 512 * sum [2^(n-1) | n <- oloc]

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
logs  message = withFile "d:/Git/game.log" AppendMode (\ fd -> hPrint fd message )

logsresult      ::  String -> IO ()
logsresult  message = withFile "d:/Git/learning.log" AppendMode (\ fd -> hPrint fd message )

playero ::  String -> IO ()
playero message = withFile "d:/Git/playero.log" AppendMode (\ fd -> hPrint fd message )
  
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
  newo = if not( isX player )then (append move oloc) else oloc
  newindex = stateindex newx newo

magicnumber :: [Int]-> Int
magicnumber l = if checkallcombination l then 15 else 14


nextvalue :: (String -> IO()) -> Player -> Int -> ( IOArray Int Double) -> BoardState-> IO (BoardState,IOArray Int Double) 
nextvalue log player move a ( BoardState xloc oloc index) =  do
  let newstate = (nextstate player ( BoardState xloc oloc index) move)
  x <- catch (readthevalue a (ReinforcementLearning.index newstate))(\(SomeException e) -> printf "Reading [%d} in Next value" index >> print e >> throwIO e)
  if (x == -1.0)
  then if ((magicnumber (ReinforcementLearning.xloc newstate)) == 15)
       then do
            (writethevalue a (ReinforcementLearning.index newstate) 0)
            log $ printf "Magic number is %d. Player X wins" (magicnumber  (ReinforcementLearning.xloc newstate))
            log $ show (ReinforcementLearning.xloc newstate)
            return (newstate,a)
       else if ((magicnumber (ReinforcementLearning.oloc newstate)) == 15)
            then do
                 (writethevalue a  (ReinforcementLearning.index newstate) 1.0)
                 playero $ printf "Magic number is %d. Player O wins" (magicnumber  (ReinforcementLearning.oloc newstate))
                 playero $ show (ReinforcementLearning.oloc newstate)
                 return (newstate,a)
            else if ((length (ReinforcementLearning.oloc newstate))+(length (ReinforcementLearning.xloc newstate)) == 9)
            then do
                 playero $ printf "Sum of Length of states is 9"
                 (writethevalue a  (ReinforcementLearning.index newstate) 0.0)
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


terminalstatep :: (String -> IO()) ->( IOArray Int Double) -> Int -> IO Bool
terminalstatep log a x = do
  y <-  catch ( readthevalue a x) (\(SomeException e) ->  print e >> printf "Read in terminalstep throws exception" >> throwIO e)
  let result = (y == fromIntegral( round y))
  do {
    if y == (-1.0) then return False else return result --Default in array is not considered in logic
    }
  
greedymove ::  (String -> IO()) ->( IOArray Int Double) ->Player -> BoardState -> IO (Int,IOArray Int Double)
greedymove log a player state = 
  let possibles = possiblemoves state in
    case possibles of
      [] -> return (0, a)
      p  -> let bestvalue = -1.0 in-- Since default value in array is -1.0
              let bestmove = (head p) in
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
  

exploratorymove :: Double -> Bool
exploratorymove r = do
  r < 0.01

gameplanrevised :: (String -> IO()) ->( IOArray Int Double) -> BoardState -> BoardState -> IO (IOArray Int Double,BoardState,Double) 
gameplanrevised log a state newstate = do 
                        exploremove a state newstate
                          where
                            exploremove :: ( IOArray Int Double) -> BoardState -> BoardState ->IO (IOArray Int Double,BoardState,Double)
                            exploremove a state newstate =
                              do
                                r <- randombetween;
                                let em = exploratorymove r in
                                  do
                                    result <- (terminalstatep log a (ReinforcementLearning.index newstate));
                                    case result of
                                      True -> do
                                        b <- update a state newstate
                                        valueofnewstate <- catch (readthevalue b (ReinforcementLearning.index newstate)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index newstate)]>> throwIO e)
                                        return (b,newstate,valueofnewstate)
                                      False -> do
                                        if em
                                          then do
                                            rm <- randommove newstate
                                            (nv,d) <- nextvalue logs O rm a newstate
                                            result1 <- (terminalstatep log d (ReinforcementLearning.index nv));
                                            valueofnewstate1 <-  catch (readthevalue d (ReinforcementLearning.index nv)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index nv)]>> throwIO e)
                                            if result1
                                              then do
                                              return (d,nv,valueofnewstate1)
                                              else do
                                              r1 <- randommove nv
                                              (ns,na) <- nextvalue logs X r1 d nv
                                              exploremove na nv ns 
                                          else do
                                            (gm,c) <- greedymove log a O newstate
                                            (nv',d') <- nextvalue logs O gm c newstate
                                            d'' <- update d' newstate nv'
                                            result2 <- (terminalstatep log d'' (ReinforcementLearning.index nv'));
                                            valueofnewstate2 <-  catch (readthevalue d'' (ReinforcementLearning.index nv')) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index nv')]>> throwIO e)
                                            if result2
                                              then do
                                              return (d'',nv',valueofnewstate2)
                                              else do
                                              r1 <- randommove nv'
                                              (ns,na) <- nextvalue logs X r1 d'' nv'
                                              exploremove na nv' ns 
  

--   "Plays 1 game against the random player. Also learns and prints.
--    :X moves first and is random.  :O learns"
game ::  (String -> IO()) ->BoardState  -> BoardState -> IOArray Int Double -> IO (IOArray Int Double,BoardState,Double) 
game log state newstate a  = do
  (newa, state, result )<-  gameplanrevised log a state newstate
  return (newa, state, result )
   
playntimes :: IOArray Int Double -> (String -> IO()) ->Int -> IO (IOArray Int Double,Double)
-- playntimes log n = do a <- createarray;
playntimes a log n = do writethevalue a 0 0.5
                        r <- (randommove (BoardState [] [] 0))
                        playtime  (BoardState [] [] 0) (nextvalue logs X r a (BoardState [] [] 0)) n 0 r
                          where
                            playtime ::  BoardState -> IO (BoardState,IOArray Int Double) -> Int -> Double -> Int -> IO (IOArray Int Double,Double)
                            playtime  s ns n acc r --finala is the consolidation for the next run
                              | n == 0 = do logsresult $ printf "Played 100 times %f  %f"  acc (acc/100.0)
                                            (_, b) <- ns 
                                            return (b,acc)
                              | n > 0 = do
                                  (boardstate, b) <- ns 
                                  (updatedarray, _, result) <- game logs s  boardstate b; 
                                  r1 <- randommove (BoardState [] [] 0)
                                  playtime (BoardState [] [] 0) (nextvalue logs X  r1 updatedarray (BoardState [] [] 0)) (n - 1) (acc + result) r1
  
numruns :: IOArray Int Double ->Int -> Int -> Int -> Int -> IO()
numruns arr n1 n bins binsize  
  | n == 0 = printf "\nPlayed numruns times"
  | n > 0 = do
      p <- createarray
      writethevalue p 0 0.5
      b <- playrepeatedly p arr n1 bins binsize
      numruns arr n1 (n -1) bins binsize

playrepeatedly ::  IOArray Int Double ->IOArray Int Double ->  Int -> Int -> Int -> IO(IOArray Int Double)
playrepeatedly a arr numrun1  numbins binsize = do 
 loop a 0 binsize
    where
      loop a i bs
        | i == numbins = let x = numrun1
                             y = numbins
                             z = binsize in
                           loop1 a x 0 y z 
        | i < numbins = do
            (b,acc) <- playntimes a logs bs;
            lastvalue <- readthevalue arr i
            writethevalue arr i (lastvalue + acc) 
            loop b (i+1) bs
        where 
        loop1 a x j y z = if j < y
                              then do
                              fv <- readthevalue arr j
                              printf " Runs %f Final Value %f Binsize %d Numruns %d \n" (fv / fromIntegral( z * x)) fv z x
                              loop1 a x (j+1) y z
                              else
                              return a


main = let numbins = 1 in 
         do
           arr <- newArray (0,numbins) 0.0;
           ReinforcementLearning.numruns arr 100 100 numbins 100 -- numruns numruns numbins binsize
           return ()
 
