
module Day24 (main) where

import qualified Data.Set as Set
import Data.List (zip5)
import Data.List.Split (chunksOf)
import Data.Set (Set)

main :: IO ()
main = do
  let example = "....##..#.#..##..#..#...."
  let full    = "#.###.....#..#.##.##..#.#"

  let s = parse example
  print s
  print (step s)

  putStrLn $ "day25, part1 (example) = " <> show (check (part1 example) 2129920)
  putStrLn $ "day25, part1 (full) = "    <> show (check (part1 full) 18859569)
check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: String -> Int
part1 str = rate $ loop Set.empty (parse str) where
  loop :: Set State -> State -> State
  loop v s = if s `elem` v then s else loop (Set.insert s v) (step s)

newtype State = State [Bool] deriving (Eq,Ord)

instance Show State where
  show (State s) =
    unlines $ chunksOf 5 $ map (\b -> if b then '#' else '.') s

parse :: String -> State
parse = State . map decode where
  decode = \case '.' -> False; '#' -> True; c -> error $ "decode: " <> show c

step :: State -> State
step (State bs) = State (map stepCell (zip5 bs bs1 bs2 bs3 bs4)) where

  bs1 = drop 5 bs ++ take 5 offs
  bs2 = concat $ map (\xs -> drop 1 xs ++ [False]) (chunksOf 5 bs)
  bs3 = concat $ map (\xs -> [False] ++ take 4 xs) (chunksOf 5 bs)
  bs4 = take 5 offs ++ take 20 bs

  offs = repeat False

  stepCell (b,b1,b2,b3,b4) =
    let neighbours = filter id [b1,b2,b3,b4] in
    if b
    then length neighbours == 1
    else length neighbours `elem` [1,2]

rate :: State -> Int
rate (State bs) = sum [ n | (b,n) <- zip bs powers, b ] where
  powers = 1 : map (2*) powers
