
module Day24 (main) where

import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import Data.Set (Set)

main :: IO ()
main = do
  let example = "....##..#.#..##..#..#...."
  let full    = "#.###.....#..#.##.##..#.#"

  --let s = parse example
  --print s
  --print (step neighbours1 s)

  putStrLn $ "day24, part1 (example) = " <> show (check (part1 example) 2129920)
  putStrLn $ "day24, part1 (full) = "    <> show (check (part1 full) 18859569)

  --_xpart2 10 example
  putStrLn $ "day24, part2 (example) = " <> show (check (part2 10 example) 99)

  _xpart2 200 full
  --putStrLn $ "day24, part2 (full) = " <> show (check (part2 200 full) 2067)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: String -> Int
part1 str = rate $ loop Set.empty (parse str) where
  loop :: Set State -> State -> State
  loop v s = if s `elem` v then s else loop (Set.insert s v) (step neighbours1 s)

_xpart2 :: Int -> String -> IO ()
_xpart2 n str = do
  let states = map numBugs (iterate (step neighbours2) (parse str))
  mapM_ print $ zip [0::Int ..] (take (n+1) states)

part2 :: Int -> String -> Int
part2 n str = do
  let state:_ = drop n $ iterate (step neighbours2) (parse str)
  numBugs state

type Level = Int
type Pos = (Level,Int,Int)
newtype State = State (Set Pos) deriving (Eq,Ord)

level0positions :: [Pos]
level0positions = [ (0, x,y) | x <- [1..5], y <- [1..5] ]

instance Show State where
  show (State set) = do
    unlines $ chunksOf 5 $ map ((\b -> if b then '#' else '.') . (`elem` set)) level0positions

numBugs :: State -> Int
numBugs (State set) = Set.size set

parse :: String -> State
parse str = do
  State (Set.fromList [ p | (c,p) <- zip str level0positions, decode c ])
  where
    decode = \case '.' -> False; '#' -> True; c -> error $ "decode: " <> show c

step :: (Pos -> [Pos]) -> State -> State
step neighbours (State set) = State set' where
  set' = Set.filter nextGen $ image neighbours set
  isOn pos = pos `elem` set
  nextGen pos = do
    let count = length $ filter isOn (neighbours pos)
    count == 1 || not (isOn pos) && count == 2

rate :: State -> Int
rate (State set) = do
  sum [ n | (pos,n) <- zip level0positions powers, isOn pos ]
  where
    isOn pos = pos `elem` set
    powers = 1 : map (2*) powers

neighbours1 :: Pos -> [Pos]
neighbours1 (_,x,y) =
  [(0,x',y') | (x',y') <- [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ], inRange x', inRange y']
  where inRange n = n >= 1 && n <= 5

neighbours2 :: Pos -> [Pos]
neighbours2 pos = right pos ++ left pos ++ up pos ++ down pos
  where
    right = \case
      (l,2,3) -> [(l+1,1,y) | y <- [1..5] ] -- in
      (l,5,_) -> [(l-1,4,3)] -- out
      (l,x,y) -> [(l,x+1,y)] -- stay

    down = \case
      (l,3,2) -> [(l+1,x,1) | x <- [1..5] ] -- in
      (l,_,5) -> [(l-1,3,4)] -- out
      (l,x,y) -> [(l,x,y+1)] -- stay

    left = \case
      (l,4,3) -> [(l+1,5,y) | y <- [1..5] ] -- in
      (l,1,_) -> [(l-1,2,3)] -- out
      (l,x,y) -> [(l,x-1,y)] -- stay

    up = \case
      (l,3,4) -> [(l+1,x,5) | x <- [1..5] ] -- in
      (l,_,1) -> [(l-1,3,2)] -- out
      (l,x,y) -> [(l,x,y-1)] -- stay

image :: Ord a => (a -> [a]) -> Set a -> Set a
image f set = Set.fromList [ y | x <- Set.elems set, y <- f x ]
