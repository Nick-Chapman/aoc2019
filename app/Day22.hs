
module Day22 (main) where

--import GHC.Int
--type Inty = Int64

type Inty = Integer

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
  putStrLn $ "day22, part2 = " <> show (check (part2 s) 74132511136410)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: String -> Inty
part1 str = applyF (makeF str) (M 10007) 2019

_xpart1 :: M -> String -> [Inty]
_xpart1 (M m) str = do
  let func = applyF (invertF (makeF str)) (M m)
  map func [0..m-1]

part2 :: String -> Inty
part2 str = do
  let m = M 119315717514047
  let r = 101741582076661
  applyF (invertF (repeatF r (makeF str))) m 2020

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

cutF :: Inty -> F
cutF a = anyShuffle 1 (-a)

dealInc :: Inty -> F
dealInc n = anyShuffle n 0


newtype M = M Inty
newtype F = F (M -> F1)
data F1 = F1 { n :: Inty, a :: Inty }

anyShuffle :: Inty -> Inty -> F
anyShuffle n a = F $ \_ -> F1 {n,a}

applyF :: F -> M -> Inty -> Inty
applyF (F f) (M m) i = do
  let F1{n,a} = f (M m)
  (n * i + a) `mod` m

seqF :: F -> F -> F
seqF (F f) (F g) = F $ \(M m) -> do
  let F1{n=n1,a=a1} = f (M m)
  let F1{n=n2,a=a2} = g (M m)
  F1 {n = n2*n1 `mod` m, a = (n2 * a1 + a2) `mod` m}

repeatF :: Inty -> F -> F
repeatF k (F f) = F $ \(M m) -> do
  let F1{n,a} = f (M m)
  let nk = pow (M m) n k
  let n' = nk
  let a' = a * (1 - nk) * moduloInverseChecked (M m) (1 - n)
  F1 { n = n', a = a' }

pow :: M -> Inty -> Inty -> Inty
pow (M m) n k =
  if k == 1 then n else do
    let h = pow (M m) n (k `div` 2)
    h * h * (if k `mod` 2 == 1 then n else 1) `mod` m

invertF :: F -> F
invertF (F f) = F $ \(M m) -> do
  let F1{n,a} = f (M m)
  let n' = moduloInverseChecked (M m) n
  F1 {n = n', a = n' * (-a) `mod` m}


moduloInverseChecked :: M -> Inty -> Inty
moduloInverseChecked (M m) a = do
  let b = moduloInverse (M m) a
  let check = a * b `mod` m
  if check == 1 then b else error $ "moduloInverse" <> show (m,a,b)

moduloInverse :: M -> Inty -> Inty
moduloInverse (M m) a =
  inverseMod a m
  where
    inverseMod :: Inty -> Inty -> Inty
    inverseMod x y = do
      let a = besout x y
      case a!!2 of
        1 -> mods (a!!0) y
        _ -> 0

    besout :: Inty -> Inty -> [Inty]
    besout x y = bBesout [1,0,x] [0,1,y] where
      bBesout u v =
        case v!!2 of
          0 -> u
          _ -> let q = div (u!!2) (v!!2) in
                 bBesout v [u!!k - q * v!!k | k <- [0..2]]

    -- | mods return positve remainder of "mod" operator
    mods :: Inty -> Inty -> Inty
    mods x p = if y >= 0 then y else y + p where y = mod x p
