
module Main (main) where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7

main :: IO ()
main = do
  args <- getArgs
  mapM_ runDay (case args of [] -> [1..7]; args -> map read args)

runDay :: Int -> IO ()
runDay day =
  [   Day1.main
    , Day2.main
    , Day3.main
    , Day4.main
    , Day5.main
    , Day6.main
    , Day7.main
    ] !! (day - 1)
