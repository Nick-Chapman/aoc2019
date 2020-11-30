
module Day5 (main) where

import Control.Monad (ap,liftM)
import Control.Monad.Identity
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do

  str2 <- readFile "input/day2.input"
  let prog2 :: [Int] = map read (splitOn "," str2)
  let (day2,_) = run 0 prog2 day2machine
  when (day2 /= 6627023) $ error "day2 machine broken"

  str5 <- readFile "input/day5.input"
  let prog5 :: [Int] = map read (splitOn "," str5)

  let ((),part1) = run 1 prog5 machine
  putStrLn $ "day5, part1 = " <> show part1 -- 2845163
  when (last part1 /= 2845163) $ error "day5, part1 broken"

  let ((),part2) = run 5 prog5 machine
  putStrLn $ "day5, part2 = " <> show part2 -- 9436229


day2machine :: Eff Int
day2machine = do Write (Pos 1) 12; Write (Pos 2) 2; machine; Read 0

machine :: Eff ()
machine = loop 0
  where
    loop pos = do
      i <- Read pos
      let u:t:rest = splitDigits i
      let op = decodeOp (10*t+u)
      let mode1:mode2:_ = map decodeMode rest
      case op of

        Halt -> return ()

        Input -> do
          target <- Read (pos+1)
          v <- GetInput
          Write (Pos target) v
          loop (pos+2)

        Output -> do
          v <- readMode (pos+1) mode1
          PutOutput v
          loop (pos+2)

        Bin f -> do
          arg1 <- readMode (pos+1) mode1
          arg2 <- readMode (pos+2) mode2
          target <- Read (pos+3)
          Write (Pos target) (f arg1 arg2)
          loop (pos+4)

        JumpIf pred -> do
          v <- readMode (pos+1) mode1
          if pred v then do
            dest <- readMode (pos+2) mode2
            loop (Pos dest)
          else loop (pos+3)


readMode :: Pos -> Mode -> Eff Int
readMode pos = \case
  Positional -> do
    v1 <- Read pos
    Read (Pos v1)
  Immediate -> do
    Read pos


-- | into (0..9)* starting at least significant digit, ending in infinite stream of 0s
splitDigits :: Int -> [Int]
splitDigits n = n `mod` 10 : splitDigits (n `div` 10)

data Op
  = Bin (Int -> Int -> Int)
  | Halt | Input | Output
  | JumpIf (Int -> Bool)

decodeOp :: Int -> Op
decodeOp = \case
  1 -> Bin (+)
  2 -> Bin (*)
  3 -> Input
  4 -> Output
  5 -> JumpIf (/= 0)
  6 -> JumpIf (== 0)
  7 -> Bin (\a b -> if a < b then 1 else 0)
  8 -> Bin (\a b -> if a == b then 1 else 0)
  99 -> Halt
  n -> error $ "unknown op: " <> show n

data Mode = Positional | Immediate

decodeMode :: Int -> Mode
decodeMode = \case
  0 -> Positional
  1 -> Immediate
  n -> error $ "unknown mode: " <> show n


data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Read :: Pos -> Eff Int
  Write :: Pos -> Int -> Eff ()
  GetInput :: Eff Int
  PutOutput :: Int -> Eff ()

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

type M a = Identity a --IO in case we need debug
data State = State { mapping :: Map Pos Int, output :: [Int] }

type Prog = [Int]

newtype Pos = Pos Int deriving (Eq,Ord,Num,Enum)

run :: Int -> Prog -> Eff a -> (a,[Int])
run theInput prog eff = (res,reverse output)
  where
    Identity (res,State{output}) = loop state0 eff

    state0 :: State
    state0 = State { mapping = Map.fromList (zip [Pos 0..] prog), output = [] }

    loop :: State -> Eff a -> M (a,State)
    loop s@State{mapping,output} = \case
      Ret x -> return (x,s)
      Bind e f -> do (v,s) <- loop s e; loop s (f v)
      Read pos -> return (fromJust $ Map.lookup pos mapping, s)
      Write pos v -> return ((), s { mapping = Map.insert pos v mapping } )
      GetInput -> return (theInput,s)
      PutOutput v -> return ((),s { output = v : output })
