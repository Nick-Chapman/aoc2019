
module Day8 (main) where

import Data.List (minimumBy)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  str8 <- readFile "/home/nic/github/advent/input/day8.input"
  putStrLn $ "day8, part1 = " <> show (check (part1 str8) 1452)
  putStrLn $ "day8, part1 = " <> part2 str8 -- PHPEU
  where
    w = 25
    h = 6
    layerSize = w * h

    part1 :: String -> Int
    part1 digits = nOnes * nTwos
      where
        nOnes = length (filter (=='1') selectedLayer)
        nTwos = length (filter (=='2') selectedLayer)
        (selectedLayer,_) = minimumBy cmp2 [ (l, length (filter (=='0') l)) | l <- layers ]
        cmp2 (_,x) (_,y) = compare x y
        layers = chunksOf layerSize digits

    part2 :: String -> String
    part2 digits = '\n' : unlines (chunksOf w composite)
      where
        composite = map pr $ foldl1 (zipWith comp) layers
        pr :: Char -> Char
        pr c = if c == '0' then ' ' else if c =='1' then '*' else '?'
        comp :: Char -> Char -> Char
        comp c1 c2 = if c1 == '2' then c2 else c1
        layers = chunksOf layerSize digits

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
