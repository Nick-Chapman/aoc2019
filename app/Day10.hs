
module Day10 (main) where

import qualified Data.Set as Set

import Data.List (maximumBy)

main :: IO ()
main = do

  let small = unlines [".#..#"
                      ,"....."
                      ,"#####"
                      ,"....#"
                      ,"...##"]

  let big = unlines
        [".#..##.###...#######"
        ,"##.############..##."
        ,".#.######.########.#"
        ,".###.#######.####.#."
        ,"#####.##.#.##.###.##"
        ,"..#####..#.#########"
        ,"####################"
        ,"#.####....###.#.#.##"
        ,"##.#################"
        ,"#####.##.###..####.."
        ,"..######..##.#######"
        ,"####.##.####...##..#"
        ,".#####..#.######.###"
        ,"##...#.##########..."
        ,"#.##########.#######"
        ,".####.#.###.###.#.##"
        ,"....##.##.###..#####"
        ,".#.#.###########.###"
        ,"#.#.#.#####.####.###"
        ,"###.##.####.##.#..##"]

  str10 <- readFile "/home/nic/github/advent/input/day10.input"

  putStrLn $ "day10, part1, small  = " <> show (part1 small)
  putStrLn $ "day10, part1, big    = " <> show (part1 big)
  putStrLn $ "day10, part1, ANSWER = " <> show (part1 str10)

  putStrLn $ "day10, part2, small  = " <> show (part2 small)
  --putStrLn $ "day10, part2, big    = " <> show (part2 big)
  --putStrLn $ "day10, part2, ANSWER = " <> show (part2 str10)

makePositions :: String -> [XY]
makePositions s = do
  (y,row) <- zip [0..] $ lines s
  (x,char) <- zip [0..] row
  case char of
    '.' -> []
    '#' -> [XY x y]
    _ -> error "bad char"

part1 :: String -> (XY,Int)
part1 s = do
  let positions :: [XY] = makePositions s
  maximumBy (compareBy snd) $ map (\p -> (p, countUnique (angles p positions))) positions
    where compareBy f a b = compare (f a) (f b)

countUnique :: [Angle] -> Int
countUnique xs = Set.size (Set.fromList xs)

angles :: XY -> [XY] -> [Angle]
angles a others = map (makeAngle a) (filter (/= a) others)

makeAngle :: XY -> XY -> Angle
makeAngle (XY x1 y1) (XY x2 y2) = do
  let x = x1 - x2
  let y = y1 - y2
  let d = Prelude.gcd x y
  if (d == 0)
    then error (show (x,y,d))
    else Angle (x `div` d) (y `div` d)

part2 :: String -> [(XY,Target)]
part2 s = do
  let (selected,_) = part1 s
  let positions :: [XY] = makePositions s
  map (\p -> (p, makeTarget selected p)) (filter (/= selected) positions)
  -- WIP, HERE. Target def is not quite correct to allow sorting.
  -- It's not distance we want, but rank-of-distance in the given angle. so need to collate & sort etc...

makeTarget :: XY -> XY -> Target
makeTarget (XY x1 y1) (XY x2 y2) = do
  let x = x1 - x2
  let y = y1 - y2
  let d = Prelude.gcd x y
  if (d == 0)
    then error (show (x,y,d))
    else Target d (realAngle x y)

realAngle :: Int -> Int -> Float
realAngle x y = do
  let rads = if y == 0 then (if x > 0 then pi/2 else -(pi/2)) else tan (float x / float (-y))
  if rads < 0 then 2 * pi + rads else rads

float :: Int -> Float
float = fromIntegral

data XY = XY Int Int deriving (Eq,Show)
data Angle = Angle Int Int deriving (Eq,Ord,Show)
data Target = Target Int Float deriving (Eq,Ord,Show)

