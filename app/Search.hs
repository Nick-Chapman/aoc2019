
module Search (
  Linkage(..), bfsWaves, BfsWave(..),

  DLink(..),dijkstra,DWave(..),
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Linkage a = Linkage { link :: a -> [a] }
data BfsWave a = BfsWave { frontier :: Set a, visited :: Set a }

{-
_bfsWaves :: forall a. Ord a => Linkage a -> [a] -> [ BfsWave a ]
_bfsWaves Linkage{link} init = loop wave0 where
  wave0 :: BfsWave a
  wave0 = BfsWave { frontier = Set.fromList init, visited = Set.empty }
  loop :: BfsWave a -> [BfsWave a]
  loop wave = wave : loop (step wave)
  step :: BfsWave a -> BfsWave a
  step BfsWave{frontier=f0,visited=v0} = do
    let visited = foldl (flip Set.insert) v0 f0
    let frontier = Set.fromList
          [ y
          | x <- Set.toList f0
          , y <- link x
          , y `Set.notMember` visited
          ]
    BfsWave { frontier, visited }
-}


bfsWaves :: forall a. Ord a => Linkage a -> [a] -> [ BfsWave a ]
bfsWaves Linkage{link} init = loop (Set.fromList init) (Set.fromList init) where
  loop :: Set a -> Set a -> [BfsWave a]
  loop frontier visited = do
    if null frontier then [] else do
      let frontier' = Set.fromList
            [ y
            | x <- Set.toList frontier
            , y <- link x
            , y `Set.notMember` visited
            ]
      let visited' = foldl (flip Set.insert) visited frontier'
      BfsWave{frontier,visited} : loop frontier' visited'


newtype DLink a = DLink { linkD :: a -> [(Int,a)] }
data DWave a = DWave { state :: a, distance :: Int, visitedD :: Set a }

dijkstra :: forall a. Ord a => DLink a -> [a] -> [ DWave a ]
dijkstra DLink{linkD} init = loop f0 v0 where

  f0 = [ (0,s) | s <- init ]
  v0 = Set.empty

  loop :: [(Int,a)] -> Set a -> [DWave a]
  loop frontier visited = do
    case frontier of
      [] -> []
      (i,state):frontier1 ->
        if state `elem` visited then loop frontier1 visited else do
          let frontier2 = [(i+j,state') | (j,state') <- linkD state ]
          let frontier' = mergeF (frontier1,frontier2)
          let visited' = Set.insert state visited
          let wave = DWave {state, distance = i, visitedD = visited'}
          wave : loop frontier' visited'

mergeF :: ([(Int,a)] , [(Int,a)]) -> [(Int,a)]
mergeF = \case
  ([],xs) -> xs
  (xs,[]) -> xs
  (xs@(x@(i,_):xs'),ys@(y@(j,_):ys'))
    | i <= j -> x : mergeF (xs',ys)
    | otherwise -> y : mergeF (xs,ys')

{-
-- defined bfs in terms of dijkstra -- to get some confidence that dijkstra is correct
__bfsWaves :: forall a. Ord a => Linkage a -> [a] -> [ BfsWave a ]
__bfsWaves Linkage{link} init = do
  let linkD s = [ (1,s') | s' <- link s ]
  let dlink = DLink {linkD}
  chunk 0 $ dijkstra dlink init
  where
    chunk :: Int -> [DWave a] -> [BfsWave a]
    chunk n = \case
      [] -> []
      w@DWave{distance=i}:waves -> do
        if (i/=n) then error "i/=n" else do
          let pred DWave{distance=j} = i==j
          let (ws1,ws2) = span pred waves
          let frontier = Set.fromList [ state | DWave{state} <- w:ws1 ]
          let DWave{visitedD=visited} = last (w:ws1)
          BfsWave {frontier,visited} : chunk (n+1) ws2
-}
