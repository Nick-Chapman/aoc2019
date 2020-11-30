
module Day10 (main) where

import qualified Data.Set as Set

import Data.List (maximumBy,groupBy,sortBy)

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

  str10 <- readFile "input/day10.input"

  putStrLn $ "day10, part1, small  = " <> show (part1 small)
  putStrLn $ "day10, part1, big    = " <> show (part1 big)
  putStrLn $ "day10, part1, ANSWER = " <> show (part1 str10)
  --putStrLn $ "day10, part2, small  = " <> show (part2 small)

  putStrLn $ "day10, part2, big(sample) = " <>
    show (check (pick [1,2,3,10,20,50,100,199,200,201,299] (part2 big))
          [(11,12),(12,1),(12,2),(12,8),(16,0),(16,9),(10,16),(9,6),(8,2),(10,9),(11,1)])

  putStrLn $ "day10, part2, ANSWER = " <>
    show (check (pick [200] (part2 str10)) [(8,29)])

  where

    pick ns verbRes  = map (\(_,(XY x y,_)) -> (x,y)) $ map (\n -> verbRes !! (n-1)) ns

    part2 s = do
      let (selected,_) = part1 s
      let positions :: [XY] = makePositions s

      let targets :: [(XY,Target)] = map (\p -> (p, makeTarget selected p)) (filter (/= selected) positions)

      let sorted_targets :: [(XY,Target)] = sortBy f targets
            where f (_,Target xy1 _) (_,Target xy2 _) = compare xy1 xy2

      let grouped :: [[(XY,Target)]] = groupBy f sorted_targets
            where f (_,Target xy1 _) (_,Target xy2 _) = (xy1==xy2)

      let sorted :: [[(XY,Target)]] = map (sortBy f) grouped
            where f (_,Target _ d1)  (_,Target _ d2) = compare d1 d2

      let shots :: [(XY,Shot)] = map makeShot $ concat $ map (zip [1::Int ..]) sorted

      let orderedShots :: [(XY,Shot)] = sortBy f shots
            where f (_,sh1) (_,sh2) = compare sh1 sh2

      zip [1::Int ..] orderedShots


makeTarget :: XY -> XY -> Target
makeTarget (XY x1 y1) (XY x2 y2) = do
  let x = x2 - x1
  let y = y2 - y1
  let d = Prelude.gcd x y
  if (d == 0)
    then error (show (x,y,d))
    else Target (XY (x `div` d) (y `div` d)) d


makeShot :: (Int,(XY,Target)) -> (XY,Shot)
makeShot (r,(a,Target (XY x y) _)) = (a,Shot r (realAngle x y) (x,y))

data Shot = Shot Int Float (Int,Int) deriving (Eq,Ord,Show)


realAngle :: Int -> Int -> Float
realAngle x y = do
  let rads =
        if x == 0 then (if y > 0 then pi else 0) else
          if y == 0 then (if x > 0 then pi/2 else -(pi/2)) else
            atan (float (-x) / float y) + (if y > 0 then pi else 0)
  if rads < 0 then 2 * pi + rads else rads

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

float :: Int -> Float
float = fromIntegral

data XY = XY Int Int deriving (Eq,Ord,Show)
data Angle = Angle Int Int deriving (Eq,Ord,Show)

data Target = Target XY Int deriving (Eq,Ord,Show)



check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
