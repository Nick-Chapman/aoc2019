
module Main (main) where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12

import qualified Assemble

main :: IO ()
main = do
  getArgs >>= \case
    ["--ass"] -> Assemble.main
    [] -> mapM_ id mains
    args -> mapM_ (\day -> mains !! (read day - 1)) args

mains :: [IO ()]
mains =
  [   Day1.main
    , Day2.main
    , Day3.main
    , Day4.main
    , Day5.main
    , Day6.main
    , Day7.main
    , Day8.main
    , Day9.main
    , Day10.main
    , Day11.main
    , Day12.main
    ]
