
module IntMachine( -- This is the Day9 machine

  Prog(..),
  loadFile,
  exec,
  execD,

  ) where

import Control.Monad (ap,liftM)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

loadFile :: String -> IO Prog
loadFile s = (Prog . map read . splitOn ",") <$> readFile s

newtype Prog = Prog [Int] deriving (Show)

type Input = [Int]
type Output = [Int]

data Act = ActOutput Int | ActExec Pos State
instance Show Act where show = prettyAct

prettyAct :: Act -> String
prettyAct = \case
  ActOutput x -> "Output: " <> show x
  ActExec _pos _state -> "pretty-exec...todo"


exec :: Prog -> Input -> Output
exec prog input = do
  let acts = run prog input machine
  [ x | act <- acts, ActOutput x <- return act ]

execD :: Prog -> Input -> [Act]
execD prog input = run prog input machine


machine :: Eff ()
machine = loop 0
  where
    loop pos = do
      PrintState pos
      i <- ReadMem pos
      let u:t:rest = splitDigits i
      let op = decodeOp (10*t+u)
      let mode1:mode2:mode3:_ = map decodeMode rest
      case op of
        Halt -> return ()
        Inp -> do
          target <- ReadMem (pos+1)
          v <- GetInput
          writeMode (Pos target) v mode1
          loop (pos+2)
        Out -> do
          v <- readMode (pos+1) mode1
          PutOutput v
          loop (pos+2)
        OpAdjustRelBase -> do
          v <- readMode (pos+1) mode1
          AdjustRelBase v
          loop (pos+2)
        Bin f -> do
          arg1 <- readMode (pos+1) mode1
          arg2 <- readMode (pos+2) mode2
          target <- ReadMem (pos+3)
          writeMode (Pos target) (f arg1 arg2) mode3
          loop (pos+4)
        JumpIf pred -> do
          v <- readMode (pos+1) mode1
          if pred v then do
            dest <- readMode (pos+2) mode2
            loop (Pos dest)
          else loop (pos+3)

readMode :: Pos -> Mode -> Eff Int
readMode pos = \case
  Positional -> do v1 <- ReadMem pos; ReadMem (Pos v1)
  Immediate -> ReadMem pos
  Relative -> do
    v1 <- ReadMem pos
    offset <- GetRelBase
    ReadMem (offset + Pos v1)

writeMode :: Pos -> Int -> Mode -> Eff ()
writeMode pos v = \case
  Positional -> WriteMem pos v
  Immediate -> error "writeMode, Immediate"
  Relative -> do
    offset <- GetRelBase
    WriteMem (offset + pos) v


-- | into (0..9)* starting at least significant digit, ending in infinite stream of 0s
splitDigits :: Int -> [Int]
splitDigits n = n `mod` 10 : splitDigits (n `div` 10)

data Op
  = Bin (Int -> Int -> Int)
  | Halt | Inp | Out
  | JumpIf (Int -> Bool)
  | OpAdjustRelBase

decodeOp :: Int -> Op
decodeOp = \case
  1 -> Bin (+)
  2 -> Bin (*)
  3 -> Inp
  4 -> Out
  5 -> JumpIf (/= 0)
  6 -> JumpIf (== 0)
  7 -> Bin (\a b -> if a < b then 1 else 0)
  8 -> Bin (\a b -> if a == b then 1 else 0)
  9 -> OpAdjustRelBase
  99 -> Halt
  n -> error $ "unknown op: " <> show n

data Mode = Positional | Immediate | Relative

decodeMode :: Int -> Mode
decodeMode = \case
  0 -> Positional
  1 -> Immediate
  2 -> Relative
  n -> error $ "unknown mode: " <> show n

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  ReadMem :: Pos -> Eff Int
  WriteMem :: Pos -> Int -> Eff ()
  GetInput :: Eff Int
  PutOutput :: Int -> Eff ()
  AdjustRelBase :: Int -> Eff ()
  GetRelBase :: Eff Pos
  PrintState :: Pos -> Eff ()

newtype Pos = Pos Int deriving (Eq,Ord,Num,Enum,Show)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data State = State { input :: [Int], memory :: Map Pos Int, relbase :: Int } deriving (Show)

run :: Prog -> Input -> Eff () -> [Act]
run (Prog prog) input0 machine = loop state0 (\() _ -> []) machine
  where
    state0 = State { input = input0, memory = Map.fromList (zip [Pos 0..] prog), relbase = 0 }
    loop :: State -> (a -> State -> [Act]) -> Eff a -> [Act]
    loop s k = \case
      Ret x -> k x s
      Bind e f -> loop s (\v s -> loop s k (f v)) e
      ReadMem pos -> k (fromMaybe 0 $ Map.lookup pos (memory s)) s
      WriteMem pos v -> k () (s { memory = Map.insert pos v (memory s) })
      PutOutput v -> ActOutput v : k () s
      AdjustRelBase v -> k () (s { relbase = v + relbase s })
      GetRelBase -> k (Pos (relbase s)) s
      GetInput ->
        case input s of
          [] -> error "run out of input"
          x:input' -> k x (s { input = input' })
      PrintState pos -> ActExec pos s : k () s
