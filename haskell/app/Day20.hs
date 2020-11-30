
module Day20 (main) where

import Data.Char (isUpper)
import Data.Maybe --(fromJust,maybeToList)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Search (bfsWaves,Linkage(..),frontier)

main :: IO ()
main = do
  example1 <- readFile "input/day20.example1"
  full <- readFile "input/day20.input"
  example3 <- readFile "input/day20.example3"

  putStrLn $ "day20, part1 (example1) = " <> show (check (part1 example1) 23)
  putStrLn $ "day20, part1 (full)     = " <> show (check (part1 full) 516)

  putStrLn $ "day20, part2 (example1) = " <> show (check (part2 example1) 26)
  putStrLn $ "day20, part2 (example3) = " <> show (check (part2 example3) 396)
  putStrLn $ "day20, part2 (full)     = " <> show (check (part2 full) 5966)

  --_xpart2 full

  return ()

part1 :: String -> Int
part1 str = do
  let w = parse str
  let (_,aa,zz) = findTeles w
  let linkage :: Linkage Pos = Linkage (stepWorld1 w)
  let (waves,_) = span (zz `notElem`) $ map frontier $ bfsWaves linkage [aa]
  length waves

_xpart1 :: String -> IO ()
_xpart1 str = do
  let w = parse str
  let (teles,aa,zz) = findTeles w
  print aa
  print zz
  mapM_ print teles
  let linkage :: Linkage Pos = Linkage (stepWorld1 w)
  let (waves,goal:_) = span (zz `notElem`) $ map frontier $ bfsWaves linkage [aa]
  mapM_ print (zip [0::Int ..] waves)
  print goal
  print (length waves)

part2 :: String -> Int
part2 str = do
  let w = parse str
  let (_,aa,zz) = findTeles w
  let linkage :: Linkage XPos = Linkage (stepWorld2 w)
  let (waves,_) = span ((0,zz) `notElem`) $ map frontier $ bfsWaves linkage [(0,aa)]
  length waves

_xpart2 :: String -> IO ()
_xpart2 str = do
  let w = parse str
  let (teles,aa,zz) = findTeles w
  print aa
  print zz
  mapM_ print teles
  let linkage :: Linkage XPos = Linkage (stepWorld2 w)
  let (ws1,ws2) = span ((0,zz) `notElem`) $ map frontier $ bfsWaves linkage [(0,aa)]
  mapM_ print (zip [0::Int ..] ws1)
  print (length ws1)
  print $ head ws2

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

type Pos = (Int,Int)
data Elem = Dot | Hash | Letter Char deriving Eq
data World = World { m :: Map Pos Elem, height :: Int, width :: Int }

newtype Label = Label String deriving Eq
data Portal = Portal { dot :: Pos, lab :: Pos }

data Tele = Tele { from :: Pos, into :: Pos } deriving Show

findTeles :: World -> ([Tele],Pos,Pos)
findTeles World{m} = (teleports,aa,zz) where

  letters :: [(Pos,Char)] = [ (p,c) | (p,Letter c) <- Map.toList m ]

  aa = head [ dot | (Portal {dot}, Label "AA") <- portals ]
  zz = head [ dot | (Portal {dot}, Label "ZZ") <- portals ]

  portals :: [(Portal,Label)] =
    [ (portal,Label [c1,c2])
    | (p1,c1) <- letters
    , (p2,c2) <- letters
    , let (x,y) = p1
    , (x+1,y) == p2
    , let p3 = (x+2,y)
    , let p0 = (x-1,y)
    , let portal = if Map.lookup p3 m == Just Dot
            then Portal { dot = p3, lab = p2 }
            else Portal { dot = p0, lab = p1 }
    ]
    ++
    [ (portal,Label [c1,c2])
    | (p1,c1) <- letters
    , (p2,c2) <- letters
    , let (x,y) = p1
    , (x,y+1) == p2
    , let p3 = (x,y+2)
    , let p0 = (x,y-1)
    , let portal = if Map.lookup p3 m == Just Dot
            then Portal { dot = p3, lab = p2 }
            else Portal { dot = p0, lab = p1 }
    ]

  teleports =
    [ tele
    | (Portal {dot, lab}, label1) <- portals
    , (Portal {dot = dot2}, label2) <- portals
    , label1 == label2
    , dot /= dot2
    , tele <- [ Tele { from = lab, into = dot2 } ]
    ]


parse :: String -> World
parse s = World{m,width,height} where
  m = Map.fromList
    [ ((x,y),elem)
    | (y,line) <- zip [0::Int ..] (lines s)
    , (x,c) <- zip [0::Int ..] line
    , Just elem <- [elemOfChar c]
    ]
  width = length $ head $ lines s
  height = length $ lines s

elemOfChar :: Char -> Maybe Elem
elemOfChar = \case
  '#' -> Just Hash
  '.' -> Just Dot
  ' ' -> Nothing
  c
    | isUpper c -> Just $ Letter c
    | otherwise -> error $ "elemOfChar:" <> show c


stepWorld1 :: World -> Pos -> [Pos]
stepWorld1 w@World{m} = step where

  (teles,_,_) = findTeles w

  teleport = Map.fromList [ (from,into) | Tele{from,into} <- teles ]

  step (x,y) =
    [ pos
    | p1 <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    , pos <- case fromJust $ Map.lookup p1 m of
        Hash -> []
        Dot -> [p1]
        Letter _ -> maybeToList $ Map.lookup p1 teleport
    ]

type Level = Int
type XPos = (Level,Pos)

stepWorld2 :: World -> XPos -> [XPos]
stepWorld2 w@World{m,height,width} = step where

  (teles,_,_) = findTeles w

  outward =
        [ t
        | t@(Tele{from=(x,y)}) <- teles
        , x == 1 || y == 1 || x == (width-2) || y == (height-2)
        ]

  inward =
        [ t
        | t@(Tele{into=(x,y)}) <- teles
        , x == 2 || y == 2 || x == (width-3) || y == (height-3)
        ]

  teleportInward = Map.fromList [ (from,into) | Tele{from,into} <- inward ]
  teleportOutward = Map.fromList [ (from,into) | Tele{from,into} <- outward ]

  step (lev,(x,y)) =
    [ pos
    | p1 <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    , pos <- case fromJust $ Map.lookup p1 m of
        Hash -> []
        Dot -> [(lev,p1)]
        Letter _ ->
          case Map.lookup p1 teleportInward of
            Just p2 -> [(lev+1,p2)]
            Nothing ->
              if lev == 0 then [] else
                case Map.lookup p1 teleportOutward of
                  Just p2 -> [(lev-1,p2)]
                  Nothing -> []
    ]
