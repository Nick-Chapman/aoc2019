
module IntMachine(
  Prog(..), Input(..), Output(..),
  exec,
  runMachine,
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

newtype Prog = Prog [Int] deriving (Show)
newtype Input = Input [Int] deriving (Show)
newtype Output = Output [Int] deriving (Show,Eq)


exec :: Prog -> Input -> Output
exec prog input =
  run prog input machine


runMachine :: Prog -> Input -> IO Output -- rename execIO
runMachine prog input = do
  if debug
    then runIO prog input machine
    else return $ run prog input machine
    where
      debug = False

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

run :: Prog -> Input -> Eff () -> Output
run (Prog prog) (Input input0) machine = Output $ loop state0 (\() _ -> []) machine
  where
    state0 = State { input = input0, memory = Map.fromList (zip [Pos 0..] prog), relbase = 0 }
    loop :: State -> (a -> State -> [Int]) -> Eff a -> [Int]
    loop s k = \case
      Ret x -> k x s
      Bind e f -> loop s (\v s -> loop s k (f v)) e
      ReadMem pos -> k (fromMaybe 0 $ Map.lookup pos (memory s)) s
      WriteMem pos v -> k () (s { memory = Map.insert pos v (memory s) })
      PutOutput v -> v : k () s
      AdjustRelBase v -> k () (s { relbase = v + relbase s })
      GetRelBase -> k (Pos (relbase s)) s
      GetInput ->
        case input s of
          [] -> error "run out of input"
          x:input' -> k x (s { input = input' })
      PrintState _ -> k () s

runIO :: Prog -> Input -> Eff () -> IO Output
runIO (Prog prog) (Input input0) machine = Output <$> loop state0 (\() _ -> return []) machine
  where
    state0 = State { input = input0, memory = Map.fromList (zip [Pos 0..] prog), relbase = 0 }
    loop :: State -> (a -> State -> IO [Int]) -> Eff a -> IO [Int]
    loop s k = \case
      Ret x -> k x s
      Bind e f -> loop s (\v s -> loop s k (f v)) e
      ReadMem pos -> do
        let v = fromMaybe 0 $ Map.lookup pos (memory s)
        putStrLn $ "ReadMem, " <> show (pos,v)
        k v s
      WriteMem pos v -> do
        putStrLn $ "WriteMem, " <> show (pos,v)
        k () (s { memory = Map.insert pos v (memory s) })
      PutOutput v -> do
        putStrLn $ "PutOutput, " <> show v
        (v :) <$> k () s
      AdjustRelBase v -> do
        let r1 = relbase s
        let r2 = v + relbase s
        putStrLn $ "AdjustRelBase, " <> show (r1,v,r2)
        k () (s { relbase = r2 })
      GetRelBase -> do
        let v = Pos (relbase s)
        putStrLn $ "GetRelBase, " <> show v
        k v s
      GetInput ->
        case input s of
          [] -> error "run out of input"
          x:input' -> do
            putStrLn $ "GetInput, " <> show x
            k x (s { input = input' })
      PrintState pos -> do
        print (pos,s)
        k () s
