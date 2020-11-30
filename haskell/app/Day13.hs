
module Day13 (main) where

import Control.Monad(when)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "input/day13.input"
  putStrLn $ "day13, part1 = " <> show (check (part1 prog) 236)
  let states = part2 prog
  when (showPictures) $ mapM_ (putStrLn . pictureState) states
  let State{game=Game{score}} = last states
  putStrLn $ "day13, part2 = " <> show (check score 11040)

    where showPictures = False

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: IM.Prog -> Int
part1 prog = do
  let output = IM.exec prog []
  let Game{tiles} = foldl updateGame game0 (chopInstructions output)
  length $ filter (== Block) $ Map.elems  tiles

part2 :: IM.Prog -> [State]
part2 (IM.Prog prog) = do
  let progHacked = IM.Prog (2 : tail prog)
  let process = IM.execP progHacked
  drive state0 [] process
 where
    drive :: State -> [Int] -> IM.Process -> [State]
    drive state qs = \case
      IM.Halt -> [state]
      IM.Internal _ p -> drive state qs p
      IM.Input f -> let (joy,state') = ai state in drive state' qs (f (encodeJoy joy))
      IM.Output q p -> case (q:qs) of
        [k,y,x] -> state : drive (updateState state (parseInstr (x,y,k))) [] p
        qs -> drive state qs p

data State = State
  { i :: Int
  , game :: Game
  , ballx :: Int
  , mex :: Int
  }

state0 :: State
state0 = State
  { i = 0
  , game = game0
  , ballx = 0
  , mex = 0
  }

pictureState :: State -> String
pictureState State{game,i,ballx,mex} = do
  let info :: String = "ball=" <> show ballx <> ", mex=" <> show mex
  unlines [ pictureGame (i,game), info ]

updateState :: State -> Instr -> State
updateState State{i,game,ballx,mex} instr =
  State
  { i = i + 1
  , game = updateGame game instr
  , ballx = case instr of PutTile (x,_) Ball -> x; _ -> ballx
  , mex = case instr of PutTile (x,_) Paddle -> x; _ -> mex
  }

ai :: State -> (Joy, State)
ai s@State{ballx,mex} = do
  let joy = if mex < ballx then R else if mex > ballx then L else N
  let mex' = encodeJoy joy + mex
  let s' = s { mex = mex' }
  (joy,s')

data Joy = L | N | R

encodeJoy :: Joy -> Int
encodeJoy = \case L -> -1; N -> 0; R -> 1

data Instr = PutTile Pos Tile | PutScore Int
type Pos = (Int,Int)
data Tile = Empty | Wall | Block | Paddle | Ball | Um deriving Eq
data Game = Game { tiles :: Map Pos Tile, score :: Int }

pictureGame :: (Int,Game) -> String
pictureGame (i,Game{tiles,score}) = do
  let xys = Map.keys tiles
  let xs = map fst xys
  let ys = map snd xys
  let xH = maximum (0:xs)
  let yH = maximum (0:ys)
  unlines $ ("[" <> show i <> "] score = " <> show score) : do
    y <- [0..yH]
    return $ do
      x <- [0..xH]
      show $ fromMaybe Um $ Map.lookup (x,y) tiles

game0 :: Game
game0 = Game { tiles = Map.empty, score = 0 }

updateGame :: Game -> Instr -> Game
updateGame s@Game{tiles} = \case
  PutScore score -> s { score }
  PutTile pos tile -> s { tiles = Map.insert pos tile tiles }

chopInstructions :: [Int] -> [Instr]
chopInstructions = \case
  [] -> []
  x:y:k:rest -> parseInstr (x,y,k) : chopInstructions rest
  _ -> error "chop3"

parseInstr :: (Int,Int,Int) -> Instr
parseInstr = \case
  (-1,0,k) -> PutScore k
  (x,y,k) -> PutTile (x,y) (parseTile k)

parseTile :: Int -> Tile
parseTile = \case
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  n -> error $ "parseTile, " <> show n

instance Show Tile where
  show = \case
    Empty -> "."
    Wall -> "#"
    Block -> "x"
    Paddle -> "~"
    Ball -> "o"
    Um -> "?"
