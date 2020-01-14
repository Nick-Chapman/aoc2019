
module Day4 (main) where

main :: IO ()
main = do
  let (a,b) = (272091,815432)
  putStrLn $ "day4, part1 = " <> show (part1 a b) -- 931
  putStrLn $ "day4, part2 = " <> show (part2 a b) -- 609

part1 :: Int -> Int -> Int
part1 min max = length $ do
  [ n |
    a <- [2..9],
    b <- [a..9],
    c <- [b..9],
    d <- [c..9],
    e <- [d..9],
    f <- [e..9],
    a==b || b==c || c==d || d==e || e==f,
    let n = foldl (\acc x -> acc*10+x) 0 [a,b,c,d,e,f],
    min<=n,
    n<=max
      ]

part2 :: Int -> Int -> Int
part2 min max = length $ do
  [ n |
    a <- [2..9],
    b <- [a..9],
    c <- [b..9],
    d <- [c..9],
    e <- [d..9],
    f <- [e..9],

              (a==b && b<c)
    || (a<b && b==c && c<d)
    || (b<c && c==d && d<e)
    || (c<d && d==e && e<f)
    || (d<e && e==f),

    let n = foldl (\acc x -> acc*10+x) 0 [a,b,c,d,e,f],
    min<=n,
    n<=max
      ]
