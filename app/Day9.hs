
module Day9 (main) where

import qualified IntMachine as IM

main :: IO ()
main = do
  let quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  prog9 <- IM.loadFile "/home/nic/github/advent/input/day9.input"
  putStrLn $ "day9, quine = " <> show (check (IM.exec (IM.Prog quine) []) quine)
  putStrLn $ "day9, part1 = " <> show (check (IM.exec prog9 [1]) [4080871669])
  putStrLn $ "day9, part2 = " <> show (check (IM.exec prog9 [2]) [75202])
  return ()

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
