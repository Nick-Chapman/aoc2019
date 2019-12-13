
module Day13 (main) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "/home/nic/github/advent/input/day13.input"
  putStrLn $ "day13, part1 = " <> show (check (part1 prog) 236)
  --putStrLn $ "day13, part2 = " <> show (part2 prog)
  let _ = part2 prog
  return ()

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: IM.Prog -> Int
part1 prog = do
  let output = IM.exec prog []
  length $ filter (== 2) $ Map.elems $ Map.fromList $ chop3 output

chop3 :: [Int] -> [(Pos,Int)]
chop3 = \case
  [] -> []
  x:y:k:rest -> ((x,y),k) : chop3 rest
  _ -> error "chop3"

every4th :: [a] -> [a]
every4th = \case
  [] -> []
  x:_:_:_:rest -> x : every4th rest
  _ -> error "every4th"


_ai :: State -> Int
_ai State{ballL=(blx,_),ball=(bx,by),paddle=(px,py)} = do
  let dir = bx-blx
  let predx = (py-by) * dir + bx
  if px > predx then -1 else if px < predx then 1 else 0

part2 :: IM.Prog -> IO ()
part2 (IM.Prog prog) = do
  let progHacked = IM.Prog (2 : tail prog)

  let output = IM.exec progHacked input
      states :: [State] = scanl update state0 (map parseTrip (chop3 output))
      input :: [Int] = 0 : map (\_s -> 0) (every4th (drop 752 states))

  mapM_ print (zip [0::Int ..] (map (\State{ball,paddle,score} -> (ball,paddle,score)) states))
  --mapM_ (putStrLn . _picture) (zip [0..] states)


data Instr = PutTile Pos Tile | PutScore Int
type Pos = (Int,Int)
data Tile = Empty | Wall | Block | Paddle | Ball | Um deriving Eq
data State = State { tiles :: Map Pos Tile
                   , score :: Int
                   , ball :: Pos
                   , ballL :: Pos
                   , paddle :: Pos }



_picture :: (Int,State) -> String
_picture (i,State{tiles,score}) = do
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

state0 :: State
state0 = State
  { tiles = Map.empty
  , score = 0
  , ballL = (-1,-1)
  , ball = (-1,-1)
  , paddle = (-1,-1) }

update :: State -> Instr -> State
update s@State{tiles,ball,ballL,paddle} = \case
  PutScore score -> s { score }
  PutTile pos tile -> do
    let (b1,b2) = if tile == Ball then (pos,ball) else (ball,ballL)
    s { tiles = Map.insert pos tile tiles
      , ball = b1
      , ballL = b2
      , paddle = if tile == Paddle then pos else paddle
      }

parseTrip :: ((Int,Int),Int) -> Instr
parseTrip (p,k) = case p of
  (-1,0) -> PutScore k
  _ -> PutTile p (parseTile k)

parseTile :: Int -> Tile
parseTile = \case
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  _ -> error "parseTile"

instance Show Tile where
  show = \case
    Empty -> "."
    Wall -> "#"
    Block -> "x"
    Paddle -> "~"
    Ball -> "o"
    Um -> "?"
