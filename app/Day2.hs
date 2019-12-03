
module Day2 (main) where

import Control.Monad (ap,liftM)
import Control.Monad.Identity
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  str <- readFile "/home/nic/github/advent/input/day2.input"
  let prog :: [Int] = map read (splitOn "," str)
  let part1 = run prog (machine 12 2)
  putStrLn $ "part1 = " <> show part1 -- 6627023
  let part2 = runs prog
  putStrLn $ "part2 = " <> show part2 -- 40,19

runs :: Prog -> [(Int,Int,Int)]
runs prog =
  [ (a,b,res) | a <- [0..99], b <- [0..99], let res = run prog (machine a b), res == 19690720 ]

machine :: Int -> Int -> Eff Int
machine a b = do Write (Pos 1) a; Write (Pos 2) b; loop 0
  where
    loop p = do
      instr <- Read p
      case instr of
        99 -> Read 0 -- and stop
        1 -> doOp (+)
        2 -> doOp (*)
        n -> error $ "bad op code: " <> show n
      where
        doOp :: (Int -> Int -> Int) -> Eff Int
        doOp f = do
          v1 <- Read (p+1)
          v2 <- Read (p+2)
          arg1 <- Read (Pos v1)
          arg2 <- Read (Pos v2)
          target <- Read (p+3)
          Write (Pos target) (f arg1 arg2)
          loop (p+4)

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Read :: Pos -> Eff Int
  Write :: Pos -> Int -> Eff ()

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

type M a = Identity a --IO in case we need debug
data State = State { mapping :: Map Pos Int }

type Prog = [Int]

newtype Pos = Pos Int deriving (Eq,Ord,Num,Enum)

run :: Prog -> Eff a -> a
run prog eff = res
  where
    Identity (res,_) = loop state0 eff

    state0 :: State
    state0 = State { mapping = Map.fromList (zip [Pos 0..] prog) }

    loop :: State -> Eff a -> M (a,State)
    loop s@State{mapping} = \case
      Ret x -> return (x,s)
      Bind e f -> do (v,s) <- loop s e; loop s (f v)
      Read pos -> return (fromJust $ Map.lookup pos mapping, s)
      Write pos v -> return ((), s { mapping = Map.insert pos v mapping } )
