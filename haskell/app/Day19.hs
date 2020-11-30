
module Day19 (main) where

import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "input/day19.input"
  -- _visualize prog
  putStrLn $ "day19, part1 = " <> show (check (part1 prog) 169)
  putStrLn $ "day19, part2 = " <> show (check (part2 prog) 7001134)

part1 :: IM.Prog -> Int
part1 prog = length $ filter (==1) $ do
  y <- [0..49]
  x <- [0..49]
  let [res] = IM.exec prog [x,y]
  return res

part2 :: IM.Prog -> Int
part2 prog = do
  let d = traceD (mkPred prog)
  let r = traceR (mkPred prog)
  let (x,y) = findBeamAtWidth d r 100
  10000 * x + y

findBeamAtWidth :: [Pos] -> [Pos] -> Int -> Pos
findBeamAtWidth d r w =
  topLeftCorner $ head $ dropWhile (\((x1,_),(x2,_)) -> x2-x1 < (w-1)) $ zip (drop (w-1) d) r
  where topLeftCorner ((x,_),(_,y)) = (x,y)

mkPred :: IM.Prog -> Pos -> Bool
mkPred prog (x,y) = do
  let [res] = IM.exec prog [x,y]
  res == 1

traceD :: (Pos -> Bool) -> [Pos]
traceD test = down (3,5) where
  down pos@(x,y) = pos : do
    if not (test pos) then error $ "down,check, " <> show pos else do
      if test (x,y+1) then down (x,y+1) else down (x+1,y+1)

traceR :: (Pos -> Bool) -> [Pos]
traceR test = right (3,5) where
  right pos@(x,y) = pos : do
    if not (test pos) then error $ "right,check, " <> show pos else do
      if test (x+1,y+1) then right (x+1,y+1) else right (x,y+1)

type Pos = (Int,Int)

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

_visualize :: IM.Prog -> IO ()
_visualize prog =
  putStrLn $ unlines $ do
  y <- [0..49]
  return $ do
    x <- [0..49]
    let [res] = IM.exec prog [x,y]
    return $
      if x == 0 then head (show (y `mod` 10)) else
        if y == 0 then head (show (x `mod` 10)) else
          if res == 1 then 'x' else if res == 0 then '.' else error "decodeRes"
