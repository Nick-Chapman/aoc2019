
module Day11 (main) where

import Data.List (maximum,minimum)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "/home/nic/github/advent/input/day11.input"
  let states1 = runRobot Part1 prog
  let answer1 = countPaintedPositions (last states1)
  putStrLn $ "day11, part1 = " <> show (check answer1 2129)
  let states2 = runRobot Part2 prog
  putStrLn $ "day 1, part2 = \n" <> visualize (last states2) -- PECKRGZL

runRobot :: Part -> IM.Prog -> [Robot]
runRobot part prog = do
  let outputs           = IM.exec prog inputs
      instructions      = chopInstructions outputs
      states :: [Robot] = scanl stepRobot (robot0 part) instructions
      sensed :: [Col]   = map currentCol states
      inputs :: [Int]   = map (\case Black -> 0; White -> 1) sensed
  states

data Robot = Robot -- State, plus knowlewdge of what is painted
  { dir :: Dir
  , pos :: Pos
  , painted :: Map Pos Col
  } deriving (Show)

visualize :: Robot -> String
visualize Robot{painted} = do
  let xys = (0,0) : Map.keys painted
  let xs = map fst xys
  let ys = map snd xys
  let xL = minimum xs
  let xH = maximum xs
  let yL = minimum ys
  let yH = maximum ys
  unlines $ do
    y <- [yL..yH]
    return $ do
      x <- [xL..xH]
      let col = fromMaybe Black $ Map.lookup (x,y) painted
      return $ case col of Black -> '.'; White -> '#'

currentCol :: Robot -> Col
currentCol Robot{pos,painted} = fromMaybe Black $ Map.lookup pos painted

countPaintedPositions :: Robot -> Int
countPaintedPositions Robot{painted} = Map.size painted

robot0 :: Part -> Robot
robot0 part = Robot { dir = U, pos = (0,0), painted }
  where painted = case part of Part1 -> Map.empty; Part2 -> Map.singleton (0,0) White

stepRobot :: Robot -> Instruction -> Robot
stepRobot Robot{dir,pos,painted} (Instruction col turn) = do
  let painted' = Map.insert pos col painted
  let dir' = doTurn turn dir
  let pos' = doMove pos dir'
  Robot {dir = dir', pos = pos', painted = painted'}

doTurn :: Turn -> Dir -> Dir
doTurn = \case
  Clock -> \case U -> R; R -> D; D -> L; L -> U
  Anti  -> \case U -> L; L -> D; D -> R; R -> U

doMove :: Pos -> Dir -> Pos
doMove (x,y) = \case U -> (x,y-1); R -> (x+1,y); D -> (x,y+1); L -> (x-1,y)

chopInstructions :: [Int] -> [Instruction]
chopInstructions = \case
  [] -> []
  [_] -> error "chopInstructions"
  c:t:rest -> Instruction (mkCol c) (mkTurn t) : chopInstructions rest

mkCol :: Int -> Col
mkCol = \case 0 -> Black; 1 -> White; _ -> error "mkCol"

mkTurn :: Int -> Turn
mkTurn = \case 0 -> Anti; 1 -> Clock; _ -> error "mkTurn"

data Instruction = Instruction Col Turn
data Col = Black | White  deriving (Show)
type Pos = (Int,Int)
data Dir = U | R | D | L deriving (Show)
data Turn = Clock | Anti

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

data Part = Part1 | Part2
