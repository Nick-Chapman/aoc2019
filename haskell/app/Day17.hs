
module Day17 (main) where

import qualified Data.Char as Char
import Data.List (intercalate)
import Data.Set (Set,intersection) -- ,(\\),union,intersection,findMin,null)
import qualified Data.Set as Set
import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "/home/nic/code/advent/input/day17.input"

  let output :: [Int] = IM.exec prog []
  let pic :: String = map Char.chr output

  let scaffold :: Set Pos = Set.fromList
        [ (x,y)
        | (y,line) <- zip [0::Int ..] (lines pic)
        , (x,c) <- zip [0::Int ..] line
        , c == '#'
        ]

  let intersections :: [Pos] = filter pred (Set.toList scaffold)
        where
          pred pos = do
            Set.size (Set.fromList (neighbors pos) `intersection` scaffold) == 4

  let [start] =
        [ (x,y)
        | (y,line) <- zip [0::Int ..] (lines pic)
        , (x,c) <- zip [0::Int ..] line
        , c == '^'
        ]

  let part1 :: Int = sum (map (uncurry (*)) intersections)
  putStrLn $ "day17, part1 = " <> show (check part1 5056)

  let full = compress (route N start scaffold)

  let (main,subA,subB,subC) = planSplit full
  let ascii = unlines [encodeMain main ,encodeSub subA ,encodeSub subB ,encodeSub subC ,"N"]
  res2 <- part2 ascii prog
  putStrLn $ "day17, part2 = " <> show (check res2 942367)
  return ()


planSplit :: [Instr] -> (Main,Sub,Sub,Sub)
planSplit = head . start
  where
    start :: [Instr] -> [(Main,Sub,Sub,Sub)]
    start is = do
      n <- [1..10]
      let (a,is') = splitAt n is
      if (sizeSub a > 20) then [] else do
        (main,b,c) <- withA a is'
        return (A:main,a,b,c)

    reuse x is =
      let (xx,is') = splitAt (length x) is in
        if x == xx then Just is' else Nothing

    withA :: Sub -> [Instr] -> [(Main,Sub,Sub)]
    withA a is =
      case reuse a is of
        Just is' -> (do (main,b,c) <- withA a is'; return (A:main,b,c)); Nothing -> do
          n <- [1..10]
          let (b,is') = splitAt n is
          if (sizeSub b > 20) then [] else do
            (main,c) <- withAB a b is'
            return ([B]++main,b,c)


    withAB :: Sub -> Sub -> [Instr] -> [(Main,Sub)]
    withAB a b is =
      case reuse a is of
        Just is' -> (do (main,c) <- withAB a b is'; return (A:main,c)); Nothing -> do
          case reuse b is of
            Just is' -> (do (main,c) <- withAB a b is'; return (B:main,c)); Nothing -> do
              n <- [1..10]
              let (c,is') = splitAt n is
              if (sizeSub b > 20) then [] else do
                main <- withABC a b c is'
                return ([C]++main,c)

    withABC :: Sub -> Sub -> Sub -> [Instr] -> [Main]
    withABC a b c is = do
      case reuse a is of
        Just is' -> (do (main) <- withABC a b c is'; return (A:main)); Nothing -> do
          case reuse b is of
            Just is' -> (do (main) <- withABC a b c is'; return (B:main)); Nothing -> do
              case reuse c is of
                Just is' -> (do (main) <- withABC a b c is'; return (C:main)); Nothing -> do
                  if (length is > 0) then [] else do -- TODO: check we drop nothing
                    return []


sizeSub :: Sub -> Int
sizeSub = length . encodeSub

encodeSub :: Sub -> String
encodeSub is = intercalate "," $ map encodeI is

encodeMain :: Main -> String
encodeMain xs = intercalate "," $ map encodeCall xs

encodeCall :: SubCall -> String
encodeCall = \case
  A -> "A"
  B -> "B"
  C -> "C"

encodeI :: Instr -> String
encodeI = \case
  L -> "L"
  R -> "R"
  Move n -> show n


data SubCall = A | B | C
type Main = [SubCall]
type Sub = [Instr]



compress :: [Instr] -> [Instr]
compress = \case
  Move a : Move b : is -> compress (Move (a+b) : is)
  [] -> []
  x:is -> x : compress is

route :: Dir -> Pos -> Set Pos -> [Instr]
route facing current scaffold0 = do
  let scaffold = Set.delete current scaffold0
  if null scaffold then [] else do
    let candNext = Set.toList (Set.fromList (neighbors current) `intersection` scaffold)
    case candNext of
      [next] -> do
        let facing' = constructStep current next
        constructTurns (facing,facing') ++ [Move 1] ++ route facing' next scaffold

      [_,_] -> do -- one step past intersection (reinstated for next cross)
        [Move 1] ++ route facing (doMove current facing) scaffold

      [_,_,_] -> do -- intersection
        [Move 1] ++ route facing (doMove current facing) (Set.insert current scaffold)
      _  ->
        error $ "unexpected #candNext (not 1, 2 or 3): " <> show candNext


constructTurns :: (Dir,Dir) -> [Instr]
constructTurns = \case
  (N,N) -> []; (E,E) -> []; (S,S) -> []; (W,W) -> [];
  (N,E) -> [R]; (E,S) -> [R]; (S,W) -> [R]; (W,N) -> [R];
  (E,N) -> [L]; (S,E) -> [L]; (W,S) -> [L]; (N,W) -> [L];
  p -> error $ "constructTurns, " <> show p

constructStep :: Pos -> Pos -> Dir
constructStep (x,y) (x',y')
  | x'==x && y'==y-1    = N
  | x'==x && y'==y+1    = S
  | x'==x+1 && y'==y    = E
  | x'==x-1 && y'==y    = W
  | otherwise           = error $ "constructStep" <> show (x,y) <> show (x',y')

data Instr = L | R | Move Int deriving (Show,Eq)


part2 :: String -> IM.Prog -> IO Int
part2 ascii (IM.Prog prog) = do
  let progWakeRobot = IM.Prog (2 : drop 1 prog)
  let output :: [Int] = IM.exec progWakeRobot (encode ascii)
  --let _pic :: String = map Char.chr (init output)
  --putStrLn pic
  let res = last output
  --putStrLn (show res)
  return res
    where
      encode :: String -> [Int]
      encode s = map Char.ord s


data Dir = N | S | W | E deriving Show
type Pos = (Int,Int)

doMove :: Pos -> Dir -> Pos
doMove (x,y) = \case N -> (x,y-1); E -> (x+1,y); S -> (x,y+1); W -> (x-1,y)

neighbors :: Pos -> [Pos]
neighbors pos = map (doMove pos) [N,S,E,W]

--image :: (Pos -> [Pos]) -> Set Pos -> Set Pos
--image f set = Set.fromList [ y | x <- Set.elems set, y <- f x ]


check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
