
module Day1 (main) where

main :: IO ()
main = do
  str <- readFile "/home/nic/github/advent/input/day1.input"
  let xs :: [Int] = map read $ words str
  let part1 = sum (map calc xs)
  putStrLn $ "part1 = " <> show part1 -- 3318632
  let part2 = sum (map icalc xs)
  putStrLn $ "part2 = " <> show part2 -- 4975084

  where
    calc :: Int -> Int
    calc x = x `div` 3 - 2

    icalc :: Int -> Int
    icalc x = sum (takeWhile (>0) (gen x))
      where
        gen x = let f = calc x in f : gen f
