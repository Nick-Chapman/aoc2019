
module Day6 (main) where

import Data.List.Split
import Data.Map.Strict as Map

main :: IO ()
main = do
  --let str = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
  --let str = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
  str <- readFile "input/day6.input"
  let dm :: DM = Map.fromList $ Prelude.map readEdge $ lines str
  putStrLn $ "day6, part1 = " <> show (fst $ part1 dm) -- 158090
  putStrLn $ "day6, part2 = " <> show (part2 dm) -- 241

readEdge :: String -> (X,X)
readEdge s = do
  let [a,b] = splitOn ")" s
  (b,a)

type X = String
type DM = Map X X -- direct map
type CM = Map X Int -- counts map
type PM = Map X [X] -- path map

part1 :: DM -> (Int,CM)
part1 dm = do
  let xs = Map.keys dm
  dists 0 Map.empty xs
  where

    dists :: Int -> CM -> [X] -> (Int,CM)
    dists acc cm = \case
      [] -> (acc,cm)
      x:xs -> do
        let (cm',v) = dist cm x
        dists (acc+v) cm' xs

    dist :: CM -> X -> (CM,Int)
    dist cm x = case Map.lookup x cm of
      Just n -> (cm,n)
      Nothing -> do
        case Map.lookup x dm of
          Nothing -> (cm,0)
          Just y -> do
            let (cm',v) = dist cm y
            let cm'' = Map.insert x (v+1) cm'
            (cm'',v+1)


part2 :: DM -> Int
part2 dm = do
  let pm = Map.empty
  let (pm1,pathY) = path pm "YOU"
  let (_,pathS) = path pm1 "SAN"
  let (endY,endS) = looseCommonPrefix (reverse pathY, reverse pathS)
  length endY + length endS - 2
  where

    looseCommonPrefix :: ([X],[X]) -> ([X],[X])
    looseCommonPrefix = \case
      p@(x:xs,y:ys) -> if x==y then looseCommonPrefix (xs,ys) else p
      p -> p

    path :: PM -> X -> (PM,[X])
    path pm x = case Map.lookup x pm of
      Just n -> (pm,n)
      Nothing -> do
        case Map.lookup x dm of
          Nothing -> (pm,[])
          Just y -> do
            let (pm',pathY) = path pm y
            let pathX = x:pathY
            let pm'' = Map.insert x pathX pm'
            (pm'',pathX)

