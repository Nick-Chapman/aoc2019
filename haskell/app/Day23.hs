
module Day23 (main) where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified IntMachine as IM

main :: IO ()
main = do
  prog <- IM.loadFile "input/day23.input"
  --_xpart1 prog
  putStrLn $ "day23, part1 = " <> show (check (part1 prog) 24954)
  --_xpart2 prog
  putStrLn $ "day23, part2 = " <> show (check (part2 prog) 17091)


part2 :: IM.Prog -> Int
part2 prog = do
  let packets = runPart2 prog
  let ys = [ y | Packet{dest=255,y} <- packets ]
  head [ y | (y,y') <- zip ys (drop 1 ys), y==y' ]

_xpart2 :: IM.Prog -> IO ()
_xpart2 prog = do
  let packets = runPart2 prog
  mapM_ print packets

runPart2 :: IM.Prog -> [Packet]
runPart2 prog = runNet2 $ initNet $ IM.execP prog

runNet2 :: Net -> [Packet]
runNet2 = loop Nothing where
  loop held net0 = do
    let (net,packetM) = stepNet net0
    case packetM of
      Just packet@Packet{dest=255} -> loop (Just packet) net
      Just packet ->
        packet : loop held (deliverPacket packet net)
      Nothing ->
        if isIdle net then
          case held of
            Nothing -> []
            Just packet -> packet : loop Nothing (deliverPacket (packet {dest = 0}) net)
        else
          loop held net


isIdle :: Net -> Bool
isIdle (_,m) = all isIdleNode $ Map.elems m

isIdleNode :: Node -> Bool
isIdleNode Node{iq,comp} = iq==[] && isWaitingForInput comp

isWaitingForInput :: IM.Process -> Bool
isWaitingForInput = \case
  IM.Output{} -> False
  IM.Input{} -> True
  IM.Halt{} -> error "halt"
  IM.Internal{} -> error "internal"


_xpart1 :: IM.Prog -> IO ()
_xpart1 prog = do
  let packets = runPart1 prog
  mapM_ print packets

part1 :: IM.Prog -> Int
part1 prog = do
  let packets = runPart1 prog
  let (_,Packet{y}:_) = span (\Packet{dest} -> dest /= 255) packets
  y

runPart1 :: IM.Prog -> [Packet]
runPart1 prog = runNet1 $ initNet $ IM.execP prog

runNet1 :: Net -> [Packet]
runNet1 net = do
  let (net',packetM) = stepNet net
  case packetM of
    Just packet -> packet : runNet1 (deliverPacket packet net')
    Nothing -> runNet1 net'


initNet :: IM.Process -> Net
initNet p = (0, Map.fromList [ (a,Node {iq = [], comp = initAddress a p}) | a <- [0..49] ])

initAddress :: Int -> IM.Process -> IM.Process
initAddress a = loop where
  loop = \case
    IM.Output{} -> error "initAddress,output"
    IM.Input f -> f a
    IM.Halt{} -> error "initAddress,halt"
    IM.Internal _ p -> loop p


type Net = (Int, Map Int Node)
data Node = Node { iq :: [Int], comp :: IM.Process }
data Packet = Packet { dest :: Int, x :: Int, y :: Int } deriving Show

stepNet :: Net -> (Net,Maybe Packet)
stepNet net = do
  let (i,m) = net
  let node = fromJust $ Map.lookup i m
  let (node',packetM) = stepNode node
  let m' = Map.insert i node' m
  let net' = ((i+1) `mod` 50, m')
  (net',packetM)


deliverPacket :: Packet -> Net -> Net
deliverPacket Packet{dest,x,y} (i,m) = do
  let Node{iq,comp} = fromJust $ Map.lookup dest m
  let nodeD' = Node {iq = iq ++ [x,y], comp }
  let m' = Map.insert dest nodeD' m
  (i,m')

stepNode :: Node -> (Node,Maybe Packet)
stepNode Node{iq,comp} = loop comp
  where
  loop :: IM.Process -> (Node,Maybe Packet)
  loop = \case
    IM.Halt -> error "loop, unexpected halt"
    IM.Internal _ p -> loop p
    IM.Input f -> case iq of
      [] -> (Node {iq = [], comp = f (-1) }, Nothing)
      x:xs -> (Node {iq = xs, comp = f x }, Nothing)
    IM.Output x p ->
      loopO [x] p

  loopO :: [Int] -> IM.Process -> (Node,Maybe Packet)
  loopO outs = \case
    IM.Halt -> error "loopO,unexpected halt"
    IM.Internal _ p -> loopO outs p
    IM.Input _ -> error "loopO, unexpected input during packet output"
    IM.Output out p ->
      case outs++[out] of
        [dest,x,y] -> (Node {iq, comp = p}, Just $ Packet {dest, x, y})
        outs' -> loopO outs' p

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
