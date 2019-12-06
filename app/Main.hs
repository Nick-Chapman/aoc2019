
module Main (main) where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

main :: IO ()
main = do
  [arg] <- getArgs
  let day :: Int = read arg
  [   Day1.main
    , Day2.main
    , Day3.main
    , Day4.main
    , Day5.main
    , Day6.main
    ] !! (day - 1)
