
module Day12 (main) where

import Data.List (transpose)
import Data.Set (Set)
import Prelude hiding (init, (++))
import qualified Data.Set as Set

main :: IO ()
main = do
  let example = [ [-1,0,2], [2,-10,-7], [4,-8,8], [3,5,-1] ]
  let myInput = [ [3,2,-6], [-13,18,10], [-8,-1,13], [5,10,4] ]
  putStrLn $ "day12, part1 (example) = " <> show (check (part1 10 example) 179)
  putStrLn $ "day12, part1 (ANSWER) = " <> show (check (part1 1000 myInput) 14780)
  putStrLn $ "day12, part2 (example,slow) = " <> show (check (part2slow example) 2772)
  putStrLn $ "day12, part2 (example) = " <> show (check (part2 example) 2772)
  putStrLn $ "day12, part2 (ANSWER) = " <> show (check (part2 myInput) 279751820342592)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

type State = [(Coord,Coord)]
type Coord = [Int]

part1 :: Int -> [[Int]] -> Int
part1 n pss = energy (iterate step (init pss) !! n)

part2slow :: [[Int]] -> Int
part2slow pss = do
  let states :: [State] = iterate step (init pss)
  let sets :: [Set State] = scanl (flip Set.insert) Set.empty states
  let sizes :: [Int] = map Set.size sets
  last (takeWhileIncreasing sizes)

takeWhileIncreasing :: [Int] -> [Int]
takeWhileIncreasing = \case
  [] -> []
  [x] -> [x]
  x:y:rest -> if y>x then x:takeWhileIncreasing (y:rest) else [x]

energy :: State -> Int
energy all = sum [ sum (map abs p) * sum (map abs v) | (p,v) <- all ]

init :: [Coord] -> State
init ps = [ (p,v) | p <- ps, let v = take (length p) (repeat 0) ]

step :: State -> State
step all =
  [ (p',v')
  | (p,v) <- all
  , let a = foldl1 (++) [ zipWith deltaD p p2 | (p2,_) <- all ]
  , let v' = v ++ a
  , let p' = p ++ v'
  ]

(++) :: Coord -> Coord -> Coord
(++) = zipWith (+)

deltaD :: Int -> Int -> Int
deltaD = \v v2 -> if v < v2 then 1 else if v == v2 then 0 else -1

--[efficient part2: compute for each dimension independently, and combine using LCM]--

type State2 = [DState]
type DState = [(Int,Int)] -- state of all the objects on a given dimension

initD :: [[Int]] -> State2
initD pss = [ map (\p -> (p,0)) ps | ps <- transpose pss ]

stepD :: DState -> DState
stepD all =
  [ (p',v')
  | (p,v) <- all
  , let a = foldl1 (+) [ deltaD p p2 | (p2,_) <- all ]
  , let v' = v + a
  , let p' = p + v'
  ]

part2 :: [[Int]] -> Int
part2 pss = foldl1 lcm -- least common multipler (in prelude would you believe!)
  [ res
  | d0 <- initD pss
  , let states :: [DState] = iterate stepD d0
  , let sets :: [Set DState] = scanl (flip Set.insert) Set.empty states
  , let sizes :: [Int] = map Set.size sets
  , let res = last (takeWhileIncreasing sizes)
  ]
