
module Day18 (main) where

import Data.Char
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(init)
import Search (Linkage(..),bfsWaves,BfsWave(..))

main :: IO ()
main = do
  let p1_x1 = unlines
        ["#########"
        ,"#b.A.@.a#"
        ,"#########"]

  let p1_x2 = unlines
        ["########################"
        ,"#f.D.E.e.C.b.A.@.a.B.c.#"
        ,"######################.#"
        ,"#d.....................#"
        ,"########################"
        ]
  let p1_x4 = unlines
        [""
        ,"#################"
        ,"#i.G..c...e..H.p#"
        ,"########.########"
        ,"#j.A..b...f..D.o#"
        ,"########@########"
        ,"#k.E..a...g..B.n#"
        ,"########.########"
        ,"#l.F..d...h..C.m#"
        ,"#################"
        ]

  let p2_x1 = unlines
        ["#######"
        ,"#a.#Cd#"
        ,"##@#@##"
        ,"#######"
        ,"##@#@##"
        ,"#cB#Ab#"
        ,"#######"
        ]

  let p2_x4 = unlines
        ["#############"
        ,"#g#f.D#..h#l#"
        ,"#F###e#E###.#"
        ,"#dCba@#@BcIJ#"
        ,"#############"
        ,"#nK.L@#@G...#"
        ,"#M###N#H###.#"
        ,"#o#m..#i#jk.#"
        ,"#############"
        ]

  putStrLn $ "day18, part1 (example1) = " <> show (check (part1 p1_x1) 8)
  putStrLn $ "day18, part1 (example2) = " <> show (check (part1 p1_x2) 86)
  putStrLn $ "day18, part1 (example4) = " <> show (check (part1 p1_x4) 136)

  putStrLn $ "day18, part2 (example1) = " <> show (check (part2 p2_x1) 8)
  putStrLn $ "day18, part2 (example4) = " <> show (check (part2 p2_x4) 72)

  _input1 <- readFile "/home/nic/code/advent/input/day18.input"
  _explore1 _input1 -- takes about 19s
  --putStrLn $ "day18, part1 (ANSWER) = " <> show (check (part1 _input1) 4590)

  _input2 <- readFile "/home/nic/code/advent/input/day18.input2"
  --_explore2 _input2 -- 2086 (takes about 10 minutes!)
  --putStrLn $ "day18, part2 (ANSWER) = " <> show (check (part2 _input2) 2086)

  return ()

part1 :: String -> Int
part1 s = length $ part1waves s

part2 :: String -> Int
part2 s = length $ part2waves s

_explore1 :: String -> IO ()
_explore1 s = explore $ part1waves s

_explore2 :: String -> IO ()
_explore2 s = explore $ part2waves s

explore :: [BfsWave s] -> IO ()
explore waves = do
  mapM_ printWave $ zip [0::Int ..] waves
  print $ length waves
  where
    printWave (i,BfsWave{frontier=f,visited=v}) = print (i,Set.size f, Set.size v)

part1waves :: String -> [BfsWave State]
part1waves = searchWaves . part1search . parseWorld

part2waves :: String -> [BfsWave State2]
part2waves = searchWaves . part2search . parseWorld


searchWaves :: Ord s => Search s -> [BfsWave s]
searchWaves Search{init,final,step} = do
  let continue BfsWave{frontier} = all (not . final) (Set.toList frontier)
  let linkage = Linkage step
  takeWhile continue $ bfsWaves linkage init

data Search s = Search
  { init :: [s]
  , final :: s -> Bool
  , step :: s -> [s]
  }


type State = (Pos,Found)
type KeyId = Char

part1search :: World -> Search State
part1search w = Search
  { init = initState w
  , final = finalState w
  , step = steps w
  }

initState :: World -> [State]
initState world = [(findEntrance world,found0)]

finalState :: World -> State -> Bool
finalState world = do
  let allKeys = mkFound $ Set.fromList [ k | (_,Key k) <- Map.toList world ]
  \(_,keys) -> keys == allKeys

steps :: World -> State -> [State]
steps w s = [ s' | dir <- [N,S,E,W], Just s' <- [stepDir w s dir] ]


type State2 = (Pos,Pos,Pos,Pos,Rob,Found)
data Rob = Rob1 | Rob2 | Rob3 | Rob4 deriving (Eq,Ord,Show) -- who may move

part2search :: World -> Search State2
part2search w = Search
  { init = initState2 w
  , final = finalState2 w
  , step = steps2 w
  }

initState2 :: World -> [State2]
initState2 world = do
  let [a,b,c,d] = [ pos | (pos,Entrance) <- Map.toList world ]
  [ (a,b,c,d,rob,found0) | rob <- anyRob ]

finalState2 :: World -> State2 -> Bool
finalState2 world = do
  let allKeys = mkFound $ Set.fromList [ k | (_,Key k) <- Map.toList world ]
  \(_,_,_,_,_,keys) -> keys == allKeys

steps2 :: World -> State2 -> [State2]
steps2 w s2  =
  [ s2'
  | dir <- [N,S,E,W]
  , let (s,up) = chooseRobot w s2
  , Just s' <- [stepDir w s dir]
  , s2' <- up s'
  ]

chooseRobot :: World -> State2 -> (State, State -> [State2])
chooseRobot w (a,b,c,d,rob,found) =
  case rob of
    Rob1 -> ((a,found), \(a,found) -> maybeSwitch w a (a,b,c,d,rob,found))
    Rob2 -> ((b,found), \(b,found) -> maybeSwitch w b (a,b,c,d,rob,found))
    Rob3 -> ((c,found), \(c,found) -> maybeSwitch w c (a,b,c,d,rob,found))
    Rob4 -> ((d,found), \(d,found) -> maybeSwitch w d (a,b,c,d,rob,found))

maybeSwitch :: World -> Pos -> State2 -> [State2]
maybeSwitch w pos (a,b,c,d,rob,found) =
  [ (a,b,c,d,rob',found) | rob' <- if onkey w pos then anyRob else [rob] ]

anyRob :: [Rob]
anyRob = [Rob1,Rob2,Rob3,Rob4]

onkey :: World -> Pos -> Bool
onkey world pos = do
  let e = fromJust $ Map.lookup pos world
  case e of
        Entrance -> False
        Floor -> False
        Key _ -> True
        Door _ -> False
        Wall -> False


--newtype Found = Found (Set KeyId) deriving (Eq,Ord)
newtype Found = Found [KeyId] deriving (Eq,Ord) -- 19s vs 28s for alt rep, on part 1

mkFound :: Set KeyId -> Found
mkFound set = Found $ sort (Set.toList set)

found0 :: Found
found0 = Found ""

insertFound :: Char -> Found -> Found
insertFound c (Found s) = if c `elem` s then Found s else Found (sort (c:s))

elemFound :: Char -> Found -> Bool
elemFound c (Found s) = c `elem` s

type World = Map Pos Elem
type Pos = (Int,Int)
data Elem = Entrance | Floor | Key KeyId | Door KeyId | Wall deriving Show

stepDir :: World -> State -> Dir -> Maybe State
stepDir world (pos0,found0) dir = do
  let pos = stepPos pos0 dir
  let e = fromJust $ Map.lookup pos world
  let found = case e of Key k -> insertFound k found0; _ -> found0
  let b = case e of
        Entrance -> True
        Floor -> True
        Key _ -> True
        Door k -> k `elemFound` found0
        Wall -> False
  if b then Just (pos,found) else Nothing

stepPos :: Pos -> Dir -> Pos
stepPos (x,y) = \case N -> (x,y-1); E -> (x+1,y); S -> (x,y+1); W -> (x-1,y)

data Dir = N | S | E | W deriving (Eq,Show)

findEntrance :: World -> Pos
findEntrance w = head [ pos | (pos,Entrance) <- Map.toList w ]

parseWorld :: String -> World
parseWorld s = Map.fromList
  [ ((x,y),elem)
  | (y,line) <- zip [0::Int ..] (lines s)
  , (x,c) <- zip [0::Int ..] line
  , let elem = elemOfChar c
  ]

elemOfChar :: Char -> Elem
elemOfChar = \case
  '#' -> Wall
  '.' -> Floor
  '@' -> Entrance
  c
    | isLower c -> Key c
    | isUpper c -> Door (toLower c)
    | otherwise -> error $ "elemOfChar:" <> show c

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
