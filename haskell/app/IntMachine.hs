
module IntMachine( -- This is the Day9 machine

  Prog(..),
  loadFile,

  Process(..),

  exec,  -- [Int] -> [Int]
  execA, -- [Int] -> [Act]
  execP, -- Process

  ) where

import Control.Monad (ap,liftM)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

newtype Prog = Prog [Int] deriving (Show)

loadFile :: String -> IO Prog
loadFile s = (Prog . map read . splitOn ",") <$> readFile s

-- | Execute a program on a list of input Ints, producing a list of output Ints
exec :: Prog -> [Int] -> [Int]
exec prog input = do
  let acts = execA prog input
  [ x | act <- acts, ActOutput x <- return act ]

-- | Execute a program on a list of input ints, producing a list of Acts
execA :: Prog -> [Int] -> [Act]
execA prog xs = loop xs (execP prog)
  where
    loop xs = \case
      Halt -> []
      Internal (pos,state) p -> ActExec pos state : loop xs p
      Output x p -> ActOutput x : loop xs p
      Input f -> case xs of
        [] -> error "run out of input"
        x:xs -> loop xs (f x)

-- | Execute a program, returning an Input/Output Process
execP :: Prog -> Process
execP = runEff theMachine

-- | A Input/Output either requests Input, or generates Output, until halting
data Process
  = Halt
  | Internal (Pos,State) Process -- ignore when not debugging!
  | Output Int Process
  | Input (Int -> Process)

-- | An Act is either an Output-Int, or an internal instruction-execution Pos/State (for debugging)
data Act = ActOutput Int | ActExec Pos State deriving Show
--instance Show Act where show = prettyAct

{-prettyAct :: Act -> String
prettyAct = \case
  ActOutput x -> "Output: " <> show x
  ActExec _pos _state -> "pretty-exec...todo"-}

theMachine :: Eff ()
theMachine = loop 0
  where
    loop pos = do
      --PrintState pos
      i <- ReadMem pos
      let u:t:rest = splitDigits i
      let op = decodeOp (10*t+u)
      let mode1:mode2:mode3:_ = map decodeMode rest
      case op of
        OpHalt -> return ()
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
  | OpHalt | Inp | Out
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
  99 -> OpHalt
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
  --PrintState :: Pos -> Eff ()

newtype Pos = Pos Int deriving (Eq,Ord,Num,Enum,Show)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data State = State { memory :: Map Pos Int, relbase :: Int } deriving (Show)

runEff :: Eff () -> Prog -> Process
runEff eff (Prog prog) = loop state0 (\() _ -> Halt) eff
  where
    state0 = State { memory = Map.fromList (zip [Pos 0..] prog), relbase = 0 }
    loop :: State -> (a -> State -> Process) -> Eff a -> Process
    loop s k = \case
      Ret x -> k x s
      Bind e f -> loop s (\v s -> loop s k (f v)) e
      ReadMem pos -> k (fromMaybe 0 $ Map.lookup pos (memory s)) s
      WriteMem pos v -> k () (s { memory = Map.insert pos v (memory s) })
      PutOutput v -> Output v (k () s)
      AdjustRelBase v -> k () (s { relbase = v + relbase s })
      GetRelBase -> k (Pos (relbase s)) s
      GetInput -> Input $ \x -> k x s
      --PrintState pos -> Internal (pos,s) (k () s)
