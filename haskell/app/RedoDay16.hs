
module RedoDay16 (main) where -- Nov 2022 (in prep for Aoc 2022)

import Prelude hiding(init)
import Data.Word (Word8)
import qualified Data.Char as Char

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

main :: IO ()
main = do
  let tag = "(redo)day16"
  let
    t i x = print (tag, i,"-->",check x $ part1 i)
  t "80871224585914546619083218645595" "24176176"
  t "19617804207202209144916044189917" "73745418"
  t "69317163492948606335995924319873" "52432133"

  s <- readFile "input/day16.input"
  print (tag++", part1", check "19239468" $ part1 s)

  pure ()

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
