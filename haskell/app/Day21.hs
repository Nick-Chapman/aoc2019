
module Day21 (main) where

import qualified Data.Char as Char
import qualified IntMachine as IM

_main :: IO ()
_main = do
  prog <- IM.loadFile "input/day21.input"
  interact (map Char.chr . IM.exec prog . map Char.ord)


main :: IO ()
main = do
  part1
  part2
  --_explore

part1 :: IO ()
part1 = do
  prog <- IM.loadFile "input/day21.input"
  codeString <- readFile  "input/day21.part1.solution"
  let output = IM.exec prog (map Char.ord codeString)
  let num = last output
  if num > 255
    then putStrLn $ "day21, part1 = " <> show (check num 19353074)
    else
    do
      putStrLn $ map Char.chr output

part2 :: IO ()
part2 = do
  prog <- IM.loadFile "input/day21.input"
  codeString <- readFile  "input/day21.part2.solution"
  let output = IM.exec prog (map Char.ord codeString)
  let num = last output
  if num > 255
    then putStrLn $ "day21, part2 = " <> show (check num 1147582556)
    else
    do
      putStrLn $ map Char.chr output

----------------------------------------------------------------------
-- rubbish below here...

_explore :: IO ()
_explore = do
  prog <- IM.loadFile "input/day21.input"
  let perms = boolPerms 0
  let _perms = [[False,True,False,False]]
  tryAttempts prog 0 perms

boolPerms :: Int -> [[Bool]]
boolPerms = \case
  0 -> [[]]
  n -> [ b:bs | b <- [False,True], bs <- boolPerms (n-1) ]

tryAttempts :: IM.Prog -> Int -> [[Bool]] -> IO ()
tryAttempts prog i = \case
  [] -> putStrLn $ "nothing worked, after " <> show i
  vars:rest -> do
    let numSensors = 9
    let form = gen numSensors vars
    putStrLn $ "--------------------------------------------------" <> show i
    print vars
    print form
    putStrLn $ "--------------------------------------------------"
    let invJ = Instruction NotK (Right J) J
    let is = compile form ++ [invJ]
    let codeString = showCode "RUN" is
    putStrLn codeString
    putStrLn $ "--------------------------------------------------"
    let output = IM.exec prog (map Char.ord codeString)
    let num = last output
    if num > 255
      then putStrLn $ "found solution: " <> show num
      else
      do
        putStrLn $ map Char.chr output
        tryAttempts prog (i+1) rest


gen :: Int -> [Bool] -> Form
gen max vs = good [] 1
  where
    lenVs = length vs
    good :: [Int] -> Int -> Form
    good xs n = if
      | n `elem` xs -> TrueF

      | n < 1 -> error "good, n<1"

      | n >= 1 && n <= max ->
        andF (SensorF (Sensor n)) (orF (good (n+4 : xs) (n+1)) (good xs (n+4)))

      | n > max && n <= max + lenVs ->
        formB (vs !! (n - max - 1))

      | otherwise ->
        TrueF

formB :: Bool -> Form
formB b = if b then TrueF else FalseF

orF :: Form -> Form -> Form
orF f1 f2 = case (f1,f2) of
  (FalseF,f2) -> f2
  (f1,FalseF) -> f1
  (TrueF,_) -> TrueF
  (_,TrueF) -> TrueF
  _ -> OrF f1 f2

andF :: Form -> Form -> Form
andF f1 f2 = case (f1,f2) of
  (TrueF,f2) -> f2
  (f1,TrueF) -> f1
  (FalseF,_) -> FalseF
  (_,FalseF) -> FalseF
  _ -> AndF f1 f2


data Form
  = SensorF Sensor
  | AndF Form Form
  | OrF Form Form
  | FalseF
  | TrueF
  deriving Show

newtype Sensor = Sensor Int -- 1..4 (part1); 1..9 (part2)

{-
compile :: Form -> [Instruction]
compile = \case
  SensorF s -> [Instruction OrK (Left s) J]
  AndF (SensorF s) form -> compile form ++ [Instruction AndK (Left s) J]
  AndF{} -> undefined
  OrF (SensorF s) form -> compile form ++ [Instruction OrK (Left s) J]
  OrF form (SensorF s) -> compile form ++ [Instruction OrK (Left s) J]
  OrF f1 f2 -> compileT f1 ++ compile f2 ++ [Instruction OrK (Right T) J]
  FalseF -> []
  TrueF -> undefined

compileT :: Form -> [Instruction]
compileT = \case
  SensorF s -> [Instruction OrK (Left s) T]
  AndF (SensorF s) form -> compileT form ++ [Instruction AndK (Left s) T]
  AndF{} -> undefined
  OrF (SensorF s) form -> compileT form ++ [Instruction OrK (Left s) T]
  OrF form (SensorF s) -> compileT form ++ [Instruction OrK (Left s) T]
  OrF x y -> error (show (x,y))
  FalseF -> []
  TrueF -> undefined
-}

newtype SP = SP [[Sensor]] -- sum of products

compile :: Form -> [Instruction]
compile = compileSP . norm

norm :: Form -> SP
norm = \case
  SensorF s -> SP[[s]]
  AndF x y -> andSP (norm x) (norm y)
  OrF x y -> do
    let SP xs = norm x
    let SP ys = norm y
    SP (xs++ys)
  FalseF -> SP []
  TrueF -> SP [[]]

andSP :: SP -> SP -> SP
andSP (SP xs) (SP ys) = SP [ x++y | x <- xs, y <- ys]

compileSP :: SP -> [Instruction]
compileSP (SP products) = do
  product <- products
  let (is,e) = compileProd product
  is ++ [Instruction OrK e J]

compileProd :: [Sensor] -> ([Instruction],Either Sensor Reg)
compileProd = \case
  [x] -> ([],Left x)
  xs -> (setT ++ [Instruction AndK (Left x) T | x <- xs], Right T)

setT :: [Instruction]
setT = [Instruction NotK (Right J) T, Instruction OrK (Right J) T]

data Reg = T | J deriving Show
data Instruction = Instruction Kind (Either Sensor Reg) Reg
data Kind = AndK | OrK | NotK

instance Show Kind where
  show = \case
    AndK -> "AND"
    OrK -> "OR"
    NotK -> "NOT"

instance Show Sensor where
  show (Sensor n) = [['A'..] !! (n-1)]

instance Show Instruction where
  show (Instruction k (Left s) r) = show k ++ " " ++ show s ++ " " ++ show r
  show (Instruction k (Right t) r) = show k ++ " " ++ show t ++ " " ++ show r

showCode :: String -> [Instruction] -> String
showCode go is = unlines (map show is ++ [go]) where


check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
