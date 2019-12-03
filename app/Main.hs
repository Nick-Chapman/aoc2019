
module Main (main) where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2

main :: IO ()
main = do
  [arg] <- getArgs
  let day :: Int = read arg
  [Day1.main, Day2.main] !! (day-1)
