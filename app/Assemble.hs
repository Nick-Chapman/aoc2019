
{-# LANGUAGE RecursiveDo #-}

module Assemble(
  main,
  ) where

import Control.Monad (ap,liftM,when)
import Control.Monad.Fix (MonadFix,mfix)
import Prelude hiding (LT,EQ)
import qualified IntMachine as IM


main :: IO ()
main = do
  -- _main_regs
  main_vis

_main_regs :: IO ()
_main_regs = do
  testA asm1 [42,13] "asm1" [110]
  testA asm2 [4,42,13,-1,1000] "asm2" [1054]
  testQ quine1 "quine1"
  testQ quine2 "quine2"
  testQ quine3 "quine3"
  testQ quineSM "quineSM"
  testA asm2_relocated [4,42,13,-1,1000] "asm2-relocated" [1054]
  testA asm2_relocated2 [4,42,13,-1,1000] "asm2-relocated2" [1054]
  testA loader (length halt' : halt' ++ []) "loader-halt" []
  testA loader (length asm1' : asm1' ++ [42,13]) "loader-asm1" [110]
    where
      (IM.Prog halt') = assemble halt
      (IM.Prog asm1') = assemble asm1


main_vis :: IO ()
main_vis = do
  putStrLn "visualize..."
  visualize asm1 [42,13]

visualize :: Asm () -> [Int] -> IO ()
visualize asm input = do
  let prog = assemble asm
  let sos  = IM.execD prog input
  mapM_ print sos


testQ :: Asm () -> String -> IO ()
testQ asm tag = do
  let prog@(IM.Prog xoutput) = assemble asm
  testP prog [] tag xoutput

testA :: Asm () -> [Int] -> String -> [Int] -> IO ()
testA asm = testP (assemble asm)


testP :: IM.Prog -> [Int] -> String -> [Int] -> IO ()
testP prog input tag expected = do
  runCheckUsingLoader tag 0 prog input expected
  --runCheckUsingLoader tag 1 prog input expected -- breaks check below
  --runCheckUsingLoader tag 2 prog input expected
  return ()

runCheckUsingLoader :: String -> Int -> IM.Prog -> [Int] -> [Int] -> IO ()
runCheckUsingLoader tag n (IM.Prog prog) input expected =
  if n <= 0 then printRunCheck tag (IM.Prog prog) input expected
  else runCheckUsingLoader ("loader+"<>tag) (n-1) (assemble loader) (length prog : prog ++ input) expected

printRunCheck :: String -> IM.Prog -> [Int] -> [Int] -> IO ()
printRunCheck tag prog input expected = do
  --print (tag, prog, input)
  let output = IM.exec prog input
  print (tag<>"/out", check output expected)
  --print (tag<>"/good", output == expected)
  return ()

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))


asm1 :: Asm () --fixed sum
asm1 = do
  let (a,b,c,d) = (Pos 100,Pos 200, Pos 300, Pos 400)
  input a
  input b
  addpp (a,b) c
  mulip (2,c) d
  outputp d
  halt

asm2 :: Asm () -- sum n-inputs, where n is given as first input
asm2 = do
  let (n,acc,i) = (Pos 55, Pos 66, Pos 77)
  input n
  loop <- here
  input i
  addpp (acc,i) acc
  decrement n
  jmpNZ n loop
  outputp acc
  halt

quine1 :: Asm () -- quine, using relative mode
quine1 = mdo
  start <- here
  let (Loc v) = finish - start
  let (n) = (Pos 55)
  set n v
  loop <- here
  output (Relative (Pos 0))
  adjRel (Immediate 1)
  decrement n
  jmpNZ n loop
  halt
  finish <- here
  return ()

quine2 :: Asm () -- quine, using relative mode, vara space allocated in code stream (needs jumping over!)
quine2 = mdo -- but not quite a quine, because by the time we print the part of the prog which is data, it has changed!
  -- so lets, allocate the space later!
  -- It still is part of thr program and so varies before being output!
  -- ok, lets add abikity to allocate unitialized space after the program
  start <- here
  let (Loc v) = finish - start
  --n <- _alloc_inline v
  --n <- _alloc_initialized v
  n <- Free; set n v
  loop <- here
  output (Relative (Pos 0))
  adjRel (Immediate 1)
  decrement n
  jmpNZ n loop
  halt
  finish <- Later here
  return ()

quine3 :: Asm () -- quine, using withSize
quine3 = do
  withSize $ \size -> do
    n <- Free; set n size
    loop <- here
    output (Relative (Pos 0))
    adjRel (Immediate 1)
    decrement n
    jmpNZ n loop
    halt

quineSM :: Asm () -- quine, using self-modifying code -- again code printed while changing make quining tricky!
quineSM = do
  withSize $ \size -> mdo
    --nop
    set outputInstructionArg 0
    start@(Loc s) <- here
    output (Positional outputInstructionArg)
    let outputInstructionArg = Pos (s+1)
    increment outputInstructionArg
    jmpNeq (Positional outputInstructionArg) (Immediate size) start
    halt

asm2_relocated :: Asm ()
asm2_relocated = mdo
  let q3 = 5000
  nop
  memcopy (Pos q1) (q2 - q1) (Pos q3)
  jmp (Loc q3)
  Loc q1 <- here
  asm2
  Loc q2 <- here
  return ()

asm2_relocated2 :: Asm ()
asm2_relocated2 = relocate_andJumpTo (Pos 5000) asm2




loader :: Asm ()
loader = mdo
  input sizeDest
  sizeDest <- relocate_andJumpTo (Pos 6000) $ do
    loop <- here
    input (Pos 0)
    Loc x <- here
    let inputDest = Pos (x-1)
    increment inputDest
    Loc y <- here
    let sizeDest = Pos (y+2)
    jmpNeq (Positional inputDest) (Immediate 567) loop
    jmp (Loc 0)
    return sizeDest
  return ()


relocate_andJumpTo :: Pos -> Asm a -> Asm a
relocate_andJumpTo (Pos q3) asm = mdo
  memcopy2 (Pos q1) (q2 - q1) (Pos q3)
  jmp (Loc q3)
  Loc q1 <- here
  res <- asm
  Loc q2 <- here
  return res


----------------------------------------------------------------------

memcopy2 :: Pos -> Int -> Pos -> Asm () -- using relative addresing, assuming set at 0
memcopy2 (Pos from) len (Pos dest) = do
  loop <- here
  Loc x <- here
  copy (Pos from) (Pos dest)
  let (copySrc,copyDest) = (Pos (x+2), Pos (x+3))
  increment copySrc
  increment copyDest
  jmpNeq (Positional copySrc) (Immediate (from+len)) loop
  return ()



memcopy :: Pos -> Int -> Pos -> Asm () -- using relative addresing, assuming set at 0
memcopy from len dest = do
  n <- Free; set n len
  loop <- here
  copyRel from dest
  adjRel (Immediate 1)
  decrement n
  jmpNZ n loop
  adjRel (Immediate (-len))
  return ()

copyRel :: Pos -> Pos -> Asm ()
copyRel from dest = emit $ Instruction Add [Relative from, Immediate 0, Relative dest]

withSize :: (Int -> Asm a) -> Asm a
withSize f = mdo
  Loc start <- here
  let size = finish - start
  res <- f size
  Loc finish <- here
  return res

jmpNeq :: Param -> Param -> Loc -> Asm ()
jmpNeq a b dest = do
  equalF a b $ \res ->
    jmpZ res dest

equalF :: Param -> Param -> (Pos -> Asm a) -> Asm a
equalF a b f = do
  dest <- Free
  --let dest = Pos 7000
  equal a b dest
  f dest

nop :: Asm ()
nop = addip (0,Pos 0) (Pos 0)

increment :: Pos -> Asm ()
increment x = addip (1,x) x

decrement :: Pos -> Asm ()
decrement x = addip (-1,x) x

set :: Pos -> Int -> Asm ()
set x v = addii (v,0) x

copy :: Pos -> Pos -> Asm ()
copy from dest = addip (0,from) dest

----------------------------------------------------------------------

_alloc_inline :: Int -> Asm Pos
_alloc_inline v = mdo
  jmp after
  Loc d <- here
  Emit [v]
  after <- here
  return $ Pos d

_alloc_initialized :: Int -> Asm Pos -- later
_alloc_initialized v = Later $ mdo
  Loc d <- here
  Emit [v]
  return $ Pos d

jmp :: Loc -> Asm ()
jmp (Loc dest) = emit $ Instruction JmpF [Immediate 0, Immediate dest]

halt :: Asm ()
halt = emit $ Instruction Halt []

input :: Pos -> Asm ()
input pos = emit $ Instruction Inp [Positional pos]

outputp :: Pos -> Asm ()
outputp pos = emit $ Instruction Out [Positional pos]

addpp :: (Pos,Pos) -> Pos -> Asm ()
addpp (p1,p2) p3 = emit $ Instruction Add [Positional p1, Positional p2, Positional p3]

addip :: (Int,Pos) -> Pos -> Asm ()
addip (i1,p2) dest = add (Immediate i1, Positional p2) dest

addii :: (Int,Int) -> Pos -> Asm ()
addii (i1,i2) dest = add (Immediate i1, Immediate i2) dest

mulip :: (Int,Pos) -> Pos -> Asm ()
mulip (i1,p2) dest = mul (Immediate i1, Positional p2) dest

jmpNZ :: Pos -> Loc -> Asm ()
jmpNZ p1 (Loc i2) = emit $ Instruction JmpT [Positional p1, Immediate i2]

jmpZ :: Pos -> Loc -> Asm ()
jmpZ p1 (Loc i2) = emit $ Instruction JmpF [Positional p1, Immediate i2]


mul :: (Param,Param) -> Pos -> Asm ()
mul (a,b) dest = emit $ Instruction Mul [a, b, Positional dest]

add :: (Param,Param) -> Pos -> Asm ()
add (a,b) dest = emit $ Instruction Add [a, b, Positional dest]

equal :: Param -> Param -> Pos -> Asm ()
equal a b dest = emit $ Instruction EQ [a, b, Positional dest]


output :: Param -> Asm ()
output arg = emit $ Instruction Out [arg]

adjRel :: Param -> Asm ()
adjRel arg = emit $ Instruction AdjRel [arg]

here :: Asm Loc
here = Here

emit :: Instruction -> Asm ()
emit i = Emit (encodeInstruction i)

----------------------------------------------------------------------

data Instruction = Instruction Op [Param]
  deriving (Show)

data Op = Add | Mul | Inp | Out | JmpT | JmpF | LT | EQ | AdjRel | Halt
  deriving (Show)

data Param = Positional Pos | Immediate Int | Relative Pos
  deriving (Show)

newtype Pos = Pos Int
  deriving (Show)

newtype Loc = Loc Int
  deriving (Show,Num)

encodeInstruction :: Instruction -> [Int]
encodeInstruction instr@(Instruction op params) = do
  when (expectedParamCount op /= length params) $ error $ "unexpected-param-count" <> show instr
  let (args,modes) = unzip $ zipWith encodeParam [2..] params
  let opcode :: Int = foldl (.) id modes $ encodeOp op
  opcode : args

encodeParam :: Int -> Param -> (Int, (Int -> Int))
encodeParam n = \case
  Positional (Pos v) -> (v, id)
  Immediate v        -> (v, \code -> code + pow10 n)
  Relative (Pos v)   -> (v, \code -> code + 2 * pow10 n)
  where
    pow10 :: Int -> Int
    pow10 n = foldl1 (*) (take n (repeat 10))

encodeOp :: Op -> Int
encodeOp = \case
  Add -> 1
  Mul -> 2
  Inp -> 3
  Out -> 4
  JmpT -> 5
  JmpF -> 6
  LT -> 7
  EQ -> 8
  AdjRel -> 9
  Halt -> 99

expectedParamCount :: Op -> Int
expectedParamCount = \case
  Add -> 3
  Mul -> 3
  Inp -> 1
  Out -> 1
  JmpT -> 2
  JmpF -> 2
  LT -> 3
  EQ -> 3
  AdjRel -> 1
  Halt -> 0

----------------------------------------------------------------------

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Mfix :: (a -> Asm a) -> Asm a
  Emit :: [Int] -> Asm ()
  Here :: Asm Loc
  Later :: Asm a -> Asm a
  Free :: Asm Pos

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = return; (<*>) = ap
instance Monad Asm where return = Ret; (>>=) = Bind
instance MonadFix Asm where mfix = Mfix

assemble :: Asm () -> IM.Prog
assemble asm = IM.Prog (xs ++ ys)
  where
    ((),xs,ys,_) = loop 0 (length xs) (length xs + length ys) asm
    loop :: Int -> Int -> Int -> Asm a -> (a,[Int],[Int],Int)
    loop p q r = \case
      Emit xs -> ((), xs,[],r)
      Here -> (Loc p, [],[],r)
      Free -> (Pos r, [],[],r+1)
      Ret x -> (x,[],[],r)
      Bind asm f -> do
        let (v1,xs1,ys1,r1) = loop p q r asm
        let (v2,xs2,ys2,r2) = loop (p + length xs1) (q + length ys1) r1 (f v1)
        (v2, xs1 ++ xs2, ys1 ++ ys2, r2)
      Mfix f -> do
        let x@(a,_,_,_) = loop p q r (f a)
        x
      Later asm -> do
        let (v,xs,ys,r1) = loop q (q + length xs) r asm
        (v,[],xs ++ ys, r1)

