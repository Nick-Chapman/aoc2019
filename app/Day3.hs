
module Day3 (main) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  -- let line1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  -- let line2 = "U62,R66,U55,R34,D71,R55,D58,R83"
  str <- readFile "/home/nic/github/advent/input/day3.input"
  let [line1,line2] = lines str
  let points1 = pointsOf line1
  let points2 = pointsOf line2
  putStrLn $ "day3, part1 = " <> show (part1 points1 points2) -- 1626
  putStrLn $ "day3, part2 = " <> show (part2 points1 points2) -- 27330

type Pos = (Int,Int)
type Step = (Dir,Int)
data Dir = L | R | U | D deriving (Show,Read)

part2 :: [Pos] -> [Pos] -> Int
part2 points1 points2 = do
  let m1 = Map.fromList $ zip points1 [0::Int ..]
  let m2 = Map.fromList $ zip points2 [0::Int ..]
  let m = Map.intersectionWith (,) m1 m2
  minimum [ n | (a,b) <- Map.elems m, let n = a+b, n>0 ]

part1 :: [Pos] -> [Pos] -> Int
part1 points1 points2 = do
  let commonPoints = Set.toList (Set.intersection (Set.fromList points1) (Set.fromList points2))
  minimum [dist | (x,y) <- commonPoints, let dist = abs x + abs y, dist > 0]

pointsOf :: String  -> [Pos]
pointsOf = scanl walk (0,0) . concatMap unitizeStep . parse
  where
    unitizeStep (dir,n) = [dir | _ <- [1..n]]

walk :: Pos -> Dir -> Pos
walk (x,y) = \case
  L -> (x-1,y)
  R -> (x+1,y)
  U -> (x,y+1)
  D -> (x,y-1)

parse :: String -> [Step]
parse s = flip map (splitOn "," s) $ \(c:cs) -> (read [c], read cs)
