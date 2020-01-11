
module Day24 (main) where

import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import Data.Set (Set)

main :: IO ()
main = do
  let example = "....##..#.#..##..#..#...."
  let full    = "#.###.....#..#.##.##..#.#"

  let s = parse example
  print s
  print (step s)

  putStrLn $ "day24, part1 (example) = " <> show (check (part1 example) 2129920)
  putStrLn $ "day24, part1 (full) = "    <> show (check (part1 full) 18859569)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: String -> Int
part1 str = rate $ loop Set.empty (parse str) where
  loop :: Set State -> State -> State
  loop v s = if s `elem` v then s else loop (Set.insert s v) (step s)

type Pos = (Int,Int)

newtype State = State (Set Pos) deriving (Eq,Ord)

instance Show State where
  show (State set) = do
    let ps = [ (x,y) | x <- [1..5], y <- [1..5] ]
    unlines $ chunksOf 5 $ map ((\b -> if b then '#' else '.') . (`elem` set)) ps

parse :: String -> State
parse str = do
  let ps = [ (x,y) | x <- [1..5], y <- [1..5] ]
  State (Set.fromList [ p | (c,p) <- zip str ps, decode c ])
  where
    decode = \case '.' -> False; '#' -> True; c -> error $ "decode: " <> show c

step :: State -> State
step (State set) = State set' where
  set' = Set.filter nextGen cand
  cand = image neighbours set
  isOn pos = pos `elem` set
  nextGen :: Pos -> Bool
  nextGen pos =
    let neighboursOn = filter isOn (neighbours pos) in
    if isOn pos
    then length neighboursOn == 1
    else length neighboursOn `elem` [1,2]

rate :: State -> Int
rate (State set) = do
  let ps = [ (x,y) | x <- [1..5], y <- [1..5] ]
  sum [ n | (pos,n) <- zip ps powers, isOn pos ]
  where
    isOn pos = pos `elem` set
    powers = 1 : map (2*) powers

neighbours :: Pos -> [Pos]
neighbours (x,y) =
  [ (x',y') | (x',y') <- [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ], inRange x', inRange y' ]
  where inRange n = n >= 1 && n <= 5

image :: Ord a => (a -> [a]) -> Set a -> Set a
image f set = Set.fromList [ y | x <- Set.elems set, y <- f x ]
