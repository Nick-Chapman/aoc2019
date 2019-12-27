
module Day22 (main) where

--import qualified Data.Set as Set

main :: IO ()
main = do
  let _x1 = unlines ["deal with increment 7" ,"deal into new stack" ,"deal into new stack"]
  let _x2 = unlines ["cut 6","deal with increment 7","deal into new stack"]
  let _x3 = unlines ["deal with increment 7", "deal with increment 9", "cut -2"]

  let _x4 = unlines
        ["deal into new stack"
        ,"cut -2"
        ,"deal with increment 7"
        ,"cut 8"
        ,"cut -4"
        ,"deal with increment 7"
        ,"cut 3"
        ,"deal with increment 9"
        ,"deal with increment 3"
        ,"cut -1"
        ]

  --print (check (_xpart1 10 _x1) [0,3,6,9,2,5,8,1,4,7])
  --print (check (_xpart1 10 _x2) [3,0,7,4,1,8,5,2,9,6])
  --print (check (_xpart1 10 _x3) [6,3,0,7,4,1,8,5,2,9])
  --print (check (_xpart1 10 _x4) [9,2,5,8,1,4,7,0,3,6])

  s <- readFile "/home/nic/code/advent/input/day22.input"
  --mapM_ print (map parse (lines s))

  --print $ Set.size $ Set.fromList $ _xpart1 10007 s

  --print $ (xpart1 10007 s) !! 2019

  putStrLn $ "day22, part1 = " <> show (check (part1 s) 3324)


part1 :: String -> Int
part1 s = do
  let m = 10007
  let steps = map parse (lines s)
  let f = compose (map (apply m) steps)
  f 2019

_xpart1 :: Int -> String -> [Int]
_xpart1 m s = do
  let steps = map parse (lines s)
  let f = compose (map (apply m) steps)
  trans $ map f [0..m-1]

trans :: [Int] -> [Int]
trans xs = [ p | i <- [0.. length xs], (x,p) <- zip xs [0..], x == i ]

compose :: [a -> a] -> a -> a
compose = foldl1 (.) . reverse

parse :: String -> Step
parse s = case words s of
  ["deal","into","new","stack"] -> Reverse
  ["deal","with","increment",n] -> DealInc (read n)
  ["cut",n] -> Cut (read n)
  _ -> error s

data Step = Reverse | Cut Int | DealInc Int deriving Show

apply :: Int -> Step -> Int -> Int
apply m = \case
  Reverse -> \i -> m - i - 1
  Cut n -> \i -> (i-n) `mod` m
  DealInc a -> do
    --let b = moduloInverseChecked m a
    \i -> (a*i) `mod` m


{-
_moduloInversePrint :: (Int,Int) -> IO ()
_moduloInversePrint (m,a) = do
  let b = moduloInverseChecked m a
  print (m,a,b)
-}



_moduloInverseChecked :: Int -> Int -> Int
_moduloInverseChecked m a = do
  let b = moduloInverse m a
  let check = (a * b) `mod` m
  if check == 1 then b else error $ "moduloInverse" <> show (m,a,b)


moduloInverse :: Int -> Int -> Int
moduloInverse m a =
  inverseMod a m
  where
    inverseMod :: Int -> Int -> Int
    inverseMod x y = do
      let a = besout x y
      case a!!2 of
        1 -> mods (a!!0) y
        _ -> 0

    besout :: Int -> Int -> [Int]
    besout x y = bBesout [1,0,x] [0,1,y] where
      bBesout u v =
        case v!!2 of
          0 -> u
          _ -> let q = div (u!!2) (v!!2) in bBesout v [u!!k - q * v!!k | k <- [0..2]]

    -- | mods return positve remainder of "mod" operator
    mods :: Int -> Int -> Int
    mods x p = if y >= 0 then y else y + p where y = mod x p



check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
