
module Search (
  Linkage(..), bfsWaves, BfsWave(..),
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Linkage a = Linkage { link :: a -> [a] }
data BfsWave a = BfsWave { frontier :: Set a, visited :: Set a }

bfsWaves :: forall a. Ord a => Linkage a -> [a] -> [ BfsWave a ]
bfsWaves Linkage{link} init = loop wave0 where

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
          , y <- link x , y `Set.notMember` visited
          ]
    BfsWave { frontier, visited }
