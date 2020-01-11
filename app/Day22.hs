
module Day22 (main) where

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

  s <- readFile "/home/nic/code/advent/input/day22.input"

  print (check (_xpart1 (M 10) _x1) [0,3,6,9,2,5,8,1,4,7])
  print (check (_xpart1 (M 10) _x2) [3,0,7,4,1,8,5,2,9,6])
  print (check (_xpart1 (M 10) _x3) [6,3,0,7,4,1,8,5,2,9])
  print (check (_xpart1 (M 10) _x4) [9,2,5,8,1,4,7,0,3,6])

  putStrLn $ "day22, part1 = " <> show (check (part1 s) 3324)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: String -> Int
part1 str = applyF (makeF str) (M 10007) 2019

_xpart1 :: M -> String -> [Int]
_xpart1 (M m) str = do
  let f = makeF str
  trans $ map (applyF f (M m)) [0..m-1]

trans :: [Int] -> [Int]
trans xs = [ p | i <- [0.. length xs], (x,p) <- zip xs [0..], x == i ]

makeF :: String -> F
makeF str = foldl1 seqF (map parseF (lines str))

parseF :: String -> F
parseF s = case words s of
  ["deal","into","new","stack"] -> reverseF
  ["deal","with","increment",a] -> dealInc (read a)
  ["cut",a] -> cutF (read a)
  _ -> error s

reverseF :: F
reverseF = anyShuffle (-1) (-1)

cutF :: Int -> F
cutF a = anyShuffle 1 (-a)

dealInc :: Int -> F
dealInc n = anyShuffle n 0

newtype M = M Int

type F = M -> F1

anyShuffle :: Int -> Int -> F
anyShuffle n a = \_ -> F1 {n,a}

applyF :: F -> M -> Int -> Int
applyF f m = applyF1 (f m) m

seqF :: F -> F -> F
seqF f g m = seqF1 m (f m) (g m)

data F1 = F1 { n :: Int, a :: Int }

applyF1 :: F1 -> M -> Int -> Int
applyF1 F1{n,a} (M m) = \i -> (n * i + a) `mod` m

seqF1 :: M -> F1 -> F1 -> F1  -- opposite order to normal `(.)` function composition
seqF1 (M m) F1{n=n1,a=a1} F1{n=n2,a=a2} = F1 {n = (n2*n1) `mod` m, a = (n2 * a1 + a2) `mod` m}


----------------------------------------------------------------------

{-
_moduloInversePrint :: (Int,Int) -> IO ()
_moduloInversePrint (m,a) = do
  let b = moduloInverseChecked m a
  print (m,a,b)
-}


{-
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

-}
