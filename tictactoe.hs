module RL where
import Control.Monad.State
import qualified Data.Map as Map
import Control.Applicative
import Graphics.Gloss
import Data.Array.IO
import Control.Monad.Reader
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

putmagicsquare = do { store "!" 2; store "2" 9;store "3" 4;
                      store "4" 7; store "5" 5;store "6" 4; 
                      store "6" 7; store "1" 5;store "8" 4; 
                    }
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

-- initialstate :: BoardState
-- initialstate =  BoardState [0,0,0] [0,0,0] 0

createarray :: IO ( IOArray Int Int)
createarray =  do {
                       arr <- newArray (512,512) 0;
                       return arr
                  }

stateindex :: [Int] -> [Int] -> Int  
stateindex xloc oloc =  let powers = powersof2 in
                          ((foldl (+) 0 [  ( powers !!n) | n <- [0..(length xloc - 1)]]) +
                          ( 512 * foldl (+) 0 [  ( powers !!n) | n <- [0..(length oloc - 1)]]))
type ArrayAccess = ReaderT  (IOArray Int Int)  IO 
type ArrayWriteAccess = ReaderT  (IOArray Int Int)  IO() 

readvalue ::  Int -> ArrayAccess Int   
readvalue x    = do 
  a <- ask
  b <- liftIO( readArray a x);    
  return b

writevalue ::  Int -> Int -> ArrayWriteAccess   
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

game  = do
 --   "Plays 1 game against the random player. Also learns and prints.
--    :X moves first and is random.  :O learns"
 let initial_state = BoardState [0,0,0] [0,0,0] 0 in
    showstate initial_state

main =  do print (runState getrow fun)
           -- getrow and getcolumn can be refactored
           -- to remove 'store' 
           let x = (runState getrow fun)
           let y = (runState getcolumn fun)

           let ms = (runState putmagicsquare fun)
           print (stateindex [1,2,3] [4,5,6])
           display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState [1,2,3] [4,5,6] 1))
           return ()


-- (defvar value-table)

-- (defvar initial-state)

-- (defun init ()
--   (setq value-table (make-array (* 512 512) :initial-element nil))
--   (setq initial-state '(nil nil 0))
--   (set-value initial-state 0.5)
--   (values))

-- (defun value (state)
--   (aref value-table (third state)))

-- (defun set-value (state value)
--   (setf (aref value-table (third state)) value))
  
-- (defun next-state (player state move)
--   "returns new state after making the indicated move by the indicated player"
--   (let ((X-moves (first state))
--         (O-moves (second state)))
--     (if (eq player :X)
--       (push move X-moves)
--       (push move O-moves))
--     (setq state (list X-moves O-moves (state-index X-moves O-moves)))
--     (when (null (value state))
--       (set-value state (cond ((any-n-sum-to-k? 3 15 X-moves)
--                               0)
--                              ((any-n-sum-to-k? 3 15 O-moves)
--                               1)
--                              ((= 9 (+ (length X-moves) (length O-moves)))
--                               0)
--                              (t 0.5))))
--     state))


-- (defun terminal-state-p (state)
--   (integerp (value state)))

-- (defvar alpha 0.5)
-- (defvar epsilon 0.01)

-- (defun possible-moves (state)
--   "Returns a list of unplayed locations"
--   (loop for i from 1 to 9 
--         unless (or (member i (first state))
--                    (member i (second state)))
--         collect i))


-- (defun random-move (state)
--   "Returns one of the unplayed locations, selected at random"
--   (let ((possible-moves (possible-moves state)))
--     (if (null possible-moves)
--       nil
--       (nth (random (length possible-moves))
--            possible-moves))))

-- (defun greedy-move (player state)
--   "Returns the move that, when played, gives the highest valued position"
--   (let ((possible-moves (possible-moves state)))
--     (if (null possible-moves)
--       nil
--       (loop with best-value = -1
--             with best-move
--             for move in possible-moves
--             for move-value = (value (next-state player state move))
--             do (when (> move-value best-value) 
--                  (setf best-value move-value)
--                  (setf best-move move))
--             finally (return best-move)))))

-- ; Now here is the main function

-- (defvar state)

-- (defun game (&optional quiet)
--   "Plays 1 game against the random player. Also learns and prints.
--    :X moves first and is random.  :O learns"
--   (setq state initial-state)
--   (unless quiet (show-state state))
--   (loop for new-state = (next-state :X state (random-move state)) 
--         for exploratory-move? = (< (random 1.0) epsilon)
--         do
--         (when (terminal-state-p new-state)
--           (unless quiet (show-state new-state))
--           (update state new-state quiet)
--           (return (value new-state)))
--         (setf new-state (next-state :O new-state 
--                                     (if exploratory-move?
--                                       (random-move new-state)
--                                       (greedy-move :O new-state))))
--         (unless exploratory-move?
--           (update state new-state quiet))
--         (unless quiet (show-state new-state))
--         (when (terminal-state-p new-state) (return (value new-state)))
--         (setq state new-state)))

-- (defun update (state new-state &optional quiet)
--   "This is the learning rule"
--   (set-value state (+ (value state)
--                       (* alpha
--                          (- (value new-state)
--                             (value state)))))
--   (unless quiet (format t "                    ~,3F" (value state))))

-- (defun run ()
--   (loop repeat 40 do (print (/ (loop repeat 100 sum (game t)) 
--                                 100.0))))

-- (defun runs (num-runs num-bins bin-size)   ; e.g., (runs 10 40 100)
--   (loop with array = (make-array num-bins :initial-element 0.0)
--         repeat num-runs do
--         (init)
--         (loop for i below num-bins do
--               (incf (aref array i)
--                     (loop repeat bin-size sum (game t))))
--         finally (loop for i below num-bins 
--                       do (print (/ (aref array i)
       
       
 
