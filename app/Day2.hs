
module Day2 (main) where

import Control.Monad (ap,liftM)

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  str <- readFile "/home/nic/github/advent/input/day2.input"
  let prog :: [Int] = map read (splitOn "," str)
  part1 <- run prog machine
  putStrLn $ "part1 = " <> show part1 -- 6627023

type Prog = [Int]

machine :: Eff Int
machine = do Write (Pos 1) 12; Write (Pos 2) 2; loop 0
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

newtype Pos = Pos Int deriving (Eq,Ord,Num,Enum)

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Read :: Pos -> Eff Int
  Write :: Pos -> Int -> Eff ()

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data State = State { mapping :: Map Pos Int }

run :: Prog -> Eff a -> IO a -- IO in case we need debug
run prog eff = fst <$> loop state0 eff
  where
    state0 :: State
    state0 = State { mapping = Map.fromList (zip [Pos 0..] prog) }

    loop :: State -> Eff a -> IO (a,State)
    loop s@State{mapping} = \case
      Ret x -> return (x,s)
      Bind e f -> do (v,s) <- loop s e; loop s (f v)
      Read pos -> return (fromJust $ Map.lookup pos mapping, s)
      Write pos v -> return ((), s { mapping = Map.insert pos v mapping } )
