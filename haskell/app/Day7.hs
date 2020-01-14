
module Day7 (main) where

import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "/home/nic/code/advent/input/day7.input"
  putStrLn $ "day7, part1 = " <> show (check (part1 prog) 21760)
  putStrLn $ "day7, part2 = " <> show (check (part2 prog) 69816958)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

part1 :: IM.Prog -> Int
part1 prog = maximum [ config settings | settings <- perms [0,1,2,3,4] ]
  where
    config settings = do
      let [a,b,c,d,e] = settings
      let [out1] = IM.exec prog [a,0]
      let [out2] = IM.exec prog [b,out1]
      let [out3] = IM.exec prog [c,out2]
      let [out4] = IM.exec prog [d,out3]
      let [out5] = IM.exec prog [e,out4]
      out5

part2 :: IM.Prog -> Int
part2 prog =
  maximum [ config settings | settings <- perms [5,6,7,8,9] ]
  where
    config settings =
      let [a,b,c,d,e] = settings in
      let out1 = IM.exec prog (a:0:out5)
          out2 = IM.exec prog (b:  out1)
          out3 = IM.exec prog (c:  out2)
          out4 = IM.exec prog (d:  out3)
          out5 = IM.exec prog (e:  out4) in
      last out5

perms :: [a] -> [[a]]
perms = \case [] -> [[]]; (x:xs) -> concatMap (inserts x) (perms xs)
  where inserts x = \case [] -> [[x]]; (y:ys) -> (x:y:ys) : map (y:) (inserts x ys)


{-
-- original Day7 machine....

_exec :: Prog -> [Int] -> [Int]
_exec = run machine

machine :: Eff ()
machine = loop 0
  where
    loop pos = do
      i <- ReadMem pos
      let u:t:rest = splitDigits i
      let op = decodeOp (10*t+u)
      let mode1:mode2:_ = map decodeMode rest
      case op of
        Halt -> return ()
        Input -> do
          target <- ReadMem (pos+1)
          v <- GetInput
          WriteMem (Pos target) v
          loop (pos+2)
        Output -> do
          v <- readMode (pos+1) mode1
          PutOutput v
          loop (pos+2)
        Bin f -> do
          arg1 <- readMode (pos+1) mode1
          arg2 <- readMode (pos+2) mode2
          target <- ReadMem (pos+3)
          WriteMem (Pos target) (f arg1 arg2)
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

newtype Prog = Prog [Int] deriving (Show)
type Input = [Int]
type Output = [Int]

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  ReadMem :: Pos -> Eff Int
  WriteMem :: Pos -> Int -> Eff ()
  GetInput :: Eff Int
  PutOutput :: Int -> Eff ()

newtype Pos = Pos Int deriving (Eq,Ord,Num,Enum)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data State = State { input :: Input, memory :: Map Pos Int }

run :: Eff () -> Prog -> Input -> Output
run eff (Prog prog) input0 = loop state0 (\() _ -> []) eff
  where
    state0 = State { input = input0, memory = Map.fromList (zip [Pos 0..] prog) }
    loop :: State -> (a -> State -> Output) -> Eff a -> Output
    loop s k = \case
      Ret x -> k x s
      Bind e f -> loop s (\v s -> loop s k (f v)) e
      ReadMem pos -> k (fromJust $ Map.lookup pos (memory s)) s
      WriteMem pos v -> k () (s { memory = Map.insert pos v (memory s) })
      PutOutput v -> v : k () s
      GetInput ->
        case input s of
          [] -> error "run out of input"
          x:input' -> k x (s { input = input' })
-}
