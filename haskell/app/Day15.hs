
module Day15 (main) where

import Control.Monad(ap,liftM,when)
import Data.Set (Set,(\\),union,intersection,findMin,null)
import Prelude hiding (floor,null)
import qualified Data.Set as Set
import qualified IntMachine as IM

doVisualize :: Bool
doVisualize = False

main :: IO ()
main = do
  prog <- IM.loadFile "input/day15.input"

  let exploration :: [(Dir,State)] = runEff prog ai

  -- visualise
  when doVisualize $ mapM_ (putStr . visualize) (zip [1::Int ..] exploration)

  -- part1...
  let (_,State{goal=Just goal, floor}) = last exploration
  let start = (0,0)
  let link a = filter (`elem` floor) (neighbors a)
  let (path,_goal) = findPath link start (Set.singleton goal)

  --print (start,goal,_goal)
  --print path
  putStrLn $ "day15, part1 = " <> show (check (length path) 366)

  let flood = takeWhile (/= floor) $ scanl1 union (findWaves link goal)

  -- note, the first element of flood contains the location of the
  -- oxygen repair system, which is reached in 0 minutes (and so
  -- shouldn't be counted), but the last element is one-before the
  -- real final state when all the floor is covered. So the length of
  -- this list *is* the number of minutes to flood fill with oxygen

  --print (map Set.size flood)
  --print (Set.size floor)
  putStrLn $ "day15, part2 = " <> show (check (length flood) 384)


check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))


visualize :: (Int, (Dir,State)) -> String
visualize (i, (dir,State{me,goal,floor,walls})) = do
  let unexplored = image neighbors floor \\ (floor `union` walls)
  let xys = Set.toList (floor `union` walls)
  let xs = map fst xys
  let ys = map snd xys
  let xL = minimum xs
  let xH = maximum xs
  let yL = minimum ys
  let yH = maximum ys
  banner <> (unlines $ map boarder $ do
    y <- [yL..yH]
    return $ do
      x <- [xL..xH]
      let p  = (x,y)
      let char
            | Just p == goal    = 'X'
            | p == me           = 'o'
            | p == (0,0)        = 'x'
            | p `elem` floor    = ' '
            | p `elem` walls    = '#'
            | p `elem` unexplored = '?'
            | otherwise         = '.'
      return char)

  where
    boarder s = "|" <> s <> "|"
    banner = "---------------------------------------------" <> show (dir,me,i) <> "\n"


ai :: Eff ()
ai = do
  beenEverywhere >>= \case
    True -> return ()
    False -> do
      (path,finalStep) <- planWalkToSomewhereUnexplored
      mapM_ (walkUnblocked path) path
      TryWalk finalStep
      ai

beenEverywhere :: Eff Bool
beenEverywhere = do
  State{floor,walls} <- GetState
  let unexplored = image neighbors floor \\ (floor `union` walls)
  return $ null unexplored

walkUnblocked :: [Dir] -> Dir -> Eff ()
walkUnblocked path dir = do
  before <- getPos
  TryWalk dir
  after <- getPos
  when (before == after) $ error $ "walkUnblocked, " <> show (dir,before,path)

getPos :: Eff Pos
getPos = do State{me} <- GetState; return me

planWalkToSomewhereUnexplored :: Eff ([Dir],Dir)
planWalkToSomewhereUnexplored = do
  State{me,floor,walls} <- GetState
  let start = me
  let unexplored = image neighbors floor \\ (floor `union` walls)
  let anyDest = floor `intersection` image neighbors unexplored
  let link a = filter (`elem` floor) (neighbors a)
  let (path,dest) = findPath link start anyDest
  let explorationChoice:_ = filter (`elem` unexplored) (neighbors dest)
  let finalStep = constructStep dest explorationChoice
  return (path,finalStep)

findPath :: (Pos -> [Pos]) -> Pos -> Set Pos -> ([Dir],Pos)
findPath link start anyDest = do
  let pred w = null (anyDest `intersection` w)
  let (waves,reachWave:_) = span pred (findWaves link start)
  let dest = findMin (anyDest `intersection` reachWave)
  (constructPath dest (reverse waves),dest)

constructPath :: Pos -> [Set Pos] -> [Dir]
constructPath dest = \case
  [] -> []
  wave:waves -> do
    let dest' = findMin (wave `intersection` Set.fromList (neighbors dest))
    constructPath dest' waves ++ [constructStep dest' dest]

findWaves :: (Pos -> [Pos]) -> Pos -> [Set Pos]
findWaves link start =
  loop (Set.singleton start)
  where
    loop :: Set Pos -> [Set Pos]
    loop frontier = frontier : loop (image link frontier)


neighbors :: Pos -> [Pos]
neighbors pos = map (doMove pos) [N,S,E,W]

image :: (Pos -> [Pos]) -> Set Pos -> Set Pos
image f set = Set.fromList [ y | x <- Set.elems set, y <- f x ]


data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  GetState :: Eff State
  TryWalk :: Dir -> Eff ()

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind


data Dir = N | S | W | E deriving Show
type Pos = (Int,Int)
data State = State { me :: Pos, goal :: Maybe Pos, floor :: Set Pos, walls :: Set Pos }

state0 :: State
state0 = State { me = (0,0), goal = Nothing, floor = Set.singleton (0,0), walls = Set.empty }

doMove :: Pos -> Dir -> Pos
doMove (x,y) = \case N -> (x,y-1); E -> (x+1,y); S -> (x,y+1); W -> (x-1,y)

constructStep :: Pos -> Pos -> Dir
constructStep (x,y) (x',y')
  | x'==x && y'==y-1    = N
  | x'==x && y'==y+1    = S
  | x'==x+1 && y'==y    = E
  | x'==x-1 && y'==y    = W
  | otherwise           = error $ "constructStep" <> show (x,y) <> show (x',y')


type DS = (Dir,State)

runEff :: IM.Prog -> Eff () -> [DS]
runEff prog eff = res
  where
    res :: [DS]
    res = loop proc0 state0 k0 eff

    proc0 :: IM.Process = IM.execP prog

    k0 :: a -> IM.Process -> State -> [DS]
    k0 = \_ _ _ -> []

    loop :: IM.Process -> State -> (a -> IM.Process -> State -> [DS]) -> Eff a -> [DS]
    loop p s k = \case
      Ret x -> k x p s
      Bind e f -> loop p s (\v p s -> loop p s k (f v)) e
      GetState -> k s p s
      TryWalk dir -> do
        let State{me,walls,floor} = s
        let dest = doMove me dir
        let (r,p') = moveDrone dir p
        let s' = case r of
              Blocked ->          s { walls = Set.insert dest walls }
              Moved ->            s { me = dest, floor = Set.insert dest floor }
              MovedFoundTarget -> s { me = dest, floor = Set.insert dest floor, goal = Just dest }
        (dir,s') : k () p' s'


moveDrone :: Dir -> IM.Process -> (Response,IM.Process)
moveDrone dir p =
  case p of
   IM.Halt -> error "moveDrone, unexpected halt"
   IM.Output _ _ -> error "moveDrone, unexpected output"
   IM.Internal _ p -> moveDrone dir p
   IM.Input f -> getResponse (f (encodeDir dir))

getResponse :: IM.Process -> (Response,IM.Process)
getResponse = \case
   IM.Halt -> error "getResponse, unexpected halt"
   IM.Input _ -> error "getResponse, unexpected input"
   IM.Internal _ p -> getResponse p
   IM.Output v p -> (decodeResponse v, p)

data Response = Blocked | Moved | MovedFoundTarget

encodeDir :: Dir -> Int
encodeDir = \case N -> 1; S -> 2; E -> 3; W -> 4

decodeResponse :: Int -> Response
decodeResponse = \case 0 -> Blocked; 1 -> Moved; 2 -> MovedFoundTarget; _ -> error "decodeResponse"
