
module RedoDay16 (main) where -- Nov 2022 (in prep for Aoc 2022)

import Prelude hiding(init)
import Data.Word (Word8)
import qualified Data.Char as Char

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

main :: IO ()
main = do
  let tag = "(redo)day16"
  s <- readFile "input/day16.input"

  let
    t i x = print (tag, i,"-->",check x $ part1 i)
  t "80871224585914546619083218645595" "24176176"
  t "19617804207202209144916044189917" "73745418"
  t "69317163492948606335995924319873" "52432133"

  print (tag++", part1", check "19239468" $ part1 s)

  let
    t2 i x = print (tag, i,"-->",check x $ part2 i)

  t2 "03036732577212944063491565474664" "84462026"
  t2 "02935109699940807407585447034323" "78725270"
  t2 "03081770884921959731165446850517" "53553731"

  -- finally an answer for part2!!
  print (tag++", part2", check "96966221" $ part2 s)

  pure ()

part2 :: String -> String
part2 s = do
  let g0x_full = rep (init s) 10000
  let k :: Int = read (take 7 s)
  let
    step :: Signal -> Signal
    step xs = fast_step xs

    steps :: Signal -> [Signal]
    steps a = a : steps (step a)

  let g0 = drop k g0x_full
  let gs = steps g0
  let g100:_ = drop 100 gs
  concat $ map show $ take 8 g100


fast_step :: [Dig] -> [Dig]
fast_step xs = scanr1 (\d s -> (d + s) `mod` 10) xs

{-
_scanSum :: [Dig] -> [Dig]
_scanSum xs0 = loop (sig xs0) xs0
  where
    loop :: Dig -> [Dig] -> [Dig]
    loop z = \case
      [] -> []
      x:xs  -> z : loop (sub z x) xs

    sub z x = (10+z-x) `mod` 10

sig :: [Dig] -> Dig -- sum digits modulo 10
sig xs =
  sum xs `mod` 10
-}

rep :: [Dig] -> Int -> [Dig]
rep xs = loop
  where
    loop = \case
      0 -> []
      n -> xs ++ loop (n-1)

----------------------------------------------------------------------

part1 :: String -> String
part1 s = do
  let g0 = init s
  let g100:_ = drop 100 (steps g0)
  let hd8 = take 8 g100
  concat $ map show $ hd8

steps :: Signal -> [Signal]
steps a = a : steps (step a)

type Signal = [Dig]
type Dig = Word8
type Pat = [Int]

init :: String -> Signal
init str = [ fromIntegral (Char.ord c - Char.ord '0') | c <- str ]

step :: [Dig] -> [Dig]
step ds =
  [ stepElem ds i
  | i <- [1::Int .. length ds]
  ]

stepElem :: [Dig] -> Int -> Dig
stepElem ds i =
  reduce [ fromIntegral d * p
         | (d,p) <- zip ds (makePatForElem i)
         ]

reduce :: [Int] -> Dig
reduce xs = do
  let n = sum xs
  fromIntegral $ if n > 0 then n `mod` 10 else (-n) `mod` 10

makePatForElem :: Int -> Pat
makePatForElem n = tail [ b | b <- basePattern, _ <- [1..n] ]

basePattern :: Pat
basePattern = [0,1,0,-1] ++ basePattern
