
module Day16 (main) where

main :: IO ()
main = do

  let ex1 = "80871224585914546619083218645595"
  putStrLn $ "day16, part1 (example1) = " <> show (check (part1 ex1) 24176176)

  str <- readFile "/home/nic/github/advent/input/day16.input"
  putStrLn $ "day16, part1 = " <> show (check (part1 str) 19239468)

  --let ex2 = "03036732577212944063491565474664"
  --putStrLn $ "day16, part2 (example2) = " <> show (part2 ex2) -- 84462026

_too_slooow_part2 :: String -> Int
_too_slooow_part2 s = collapse $ take 8 (drop offset gen100)
  where
    offset = collapse (take 7 gen0)
    gen100 = gen 100
    n = length gen0
    gen0 = concat $ take 10000 $ repeat [ read [c] | c <- s ]
    gen :: Int -> [Int]
    gen a = if a == 0 then gen0 else do
      let prevGen = gen (a-1)
      [ ((`mod` 10) . abs) $ sum $ zipWith (*) (xpat i) prevGen | i <- [1..n] ]

part1 :: String -> Int
part1 s = collapse $ take 8 (gen 100)
  where
    n = length s
    gen0 = [ read [c] | c <- s ]
    gen :: Int -> [Int]
    gen a = if a == 0 then gen0 else do
      let prevGen = gen (a-1)
      [ ((`mod` 10) . abs) $ sum $ zipWith (*) (xpat i) prevGen | i <- [1..n] ]

xpat :: Int -> [Int]
xpat i = drop 1 $ cycle $ concatMap (\p -> take i (repeat p)) [0,1,0,-1]

collapse :: [Int] -> Int
collapse = foldl (\acc d -> 10*acc+d) 0

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
