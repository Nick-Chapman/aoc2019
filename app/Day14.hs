
module Day14 (main) where

import Data.List.Split (splitOn)

import Data.Map (Map)
import Data.Maybe (fromMaybe,fromJust)
import qualified Data.Map.Strict as Map

import Data.List (maximumBy)
import Data.List.Extra (trim)

main :: IO ()
main = do

  let x1 = unlines [
        "10 ORE => 10 A",
        "1 ORE => 1 B",
        "7 A, 1 B => 1 C",
        "7 A, 1 C => 1 D",
        "7 A, 1 D => 1 E",
        "7 A, 1 E => 1 FUEL"]

  let x2 = unlines [
        "9 ORE => 2 A",
        "8 ORE => 3 B",
        "7 ORE => 5 C",
        "3 A, 4 B => 1 AB",
        "5 B, 7 C => 1 BC",
        "4 C, 1 A => 1 CA",
        "2 AB, 3 BC, 4 CA => 1 FUEL"]

  let x5 = unlines [
        "171 ORE => 8 CNZTR",
        "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
        "114 ORE => 4 BHXH",
        "14 VRPVC => 6 BMBT",
        "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL",
        "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
        "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
        "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
        "5 BMBT => 4 WPTQ",
        "189 ORE => 9 KTJDG",
        "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
        "12 VRPVC, 27 CNZTR => 2 XDBXC",
        "15 KTJDG, 12 BHXH => 5 XCVML",
        "3 BHXH, 2 VRPVC => 7 MZWV",
        "121 ORE => 7 VRPVC",
        "7 XCVML => 6 RJRHP",
        "5 BHXH, 4 VRPVC => 5 LTCX"]

  input <- readFile "/home/nic/github/advent/input/day14.input"

  let _ = explore x2

  putStrLn $ "day14, part1 (example 1) = " <> show (check (part1 x1) 31)
  putStrLn $ "day14, part1 (example 2) = " <> show (check (part1 x2) 165)
  putStrLn $ "day14, part1 (example 5) = " <> show (check (part1 x5) 2210736)
  putStrLn $ "day14, part1 (ANSWER) = " <> show (check (part1 input) 443537)



part1 :: String -> Int
part1 s = do
  let reactions = parseReactions s
  let [("ORE",n)] = Map.toList $ last $ unroll reactions
  n

explore :: String -> IO ()
explore s = do
  let reactions = parseReactions s
  print (dfs reactions)
  mapM_ print (unroll reactions)


parseReactions :: String -> Reactions
parseReactions s = Map.fromList (map parseLine (lines s))
  where
    parseLine :: String -> (Chem,(Quantity,Inputs))
    parseLine s = do
      let [s1,s2] = splitOn "=>" s
      let inputs = Map.fromList $ map parseQC (splitOn "," s1)
      let (c,q) = parseQC s2
      (c,(q,inputs))

    parseQC :: String -> (Chem,Quantity)
    parseQC s = do
      let [s1,s2] = splitOn " " (trim s)
      (s2, read s1)

type Reactions = Map Chem (Quantity,Inputs)
type Inputs = Map Chem Quantity
type Chem = String
type Quantity = Int

scaleInputs :: Int -> Inputs -> Inputs
scaleInputs n = Map.map (*n)

addInputs :: Inputs -> Inputs -> Inputs
addInputs = Map.unionWith (+)

unroll :: Reactions -> [Inputs]
unroll reactions = steps (Map.fromList [("FUEL",1)])  where

  steps :: Inputs -> [Inputs]
  steps inputs =
    inputs : (if Map.keys inputs == ["ORE"] then [] else steps (step inputs))

  step :: Inputs -> Inputs
  step inputs = do
    let chem = latest (Map.keys inputs)
    let qNeeded = fromJust $ Map.lookup chem inputs
    let reducedInputs = Map.delete chem inputs
    let (qProduced,moreInputs) = fromJust $ Map.lookup chem reactions
    let mult = 1+ (qNeeded-1) `div` qProduced
    scaleInputs mult moreInputs `addInputs` reducedInputs

  latest :: [Chem] -> Chem
  latest xs = do
    let ps = map (\x -> (x, fromMaybe 0 $ Map.lookup x m)) xs
    let (chem,_) = maximumBy by2 ps
    chem
      where by2 (_,x) (_,y) = compare x y

  m :: Map Chem Int
  m = Map.fromList $ zip (reverse (dfs reactions)) [1..]


dfs :: Reactions -> [Chem]
dfs reactions = walk [] "FUEL"
  where
    walk :: [Chem] -> Chem -> [Chem]
    walk acc = \case
      "ORE" -> acc
      x -> do
        let ys = Map.keys $ snd $ fromJust $ Map.lookup x reactions
        let acc1 = walks acc ys
        if x `elem` acc1 then acc1 else x:acc1

    walks :: [Chem] ->[Chem] -> [Chem]
    walks acc = \case
      [] -> acc
      x:xs -> walks (walk acc x) xs



check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
