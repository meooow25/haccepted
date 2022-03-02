module KruskalBench where

import Control.Monad
import Control.Monad.Random

import Criterion

import Kruskal ( WEdge(..), kruskal )
import Util ( evalR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Kruskal"
    [ -- Run Kruskal's algorithm on a graph with n edges and n/4 vertices
      bgroup "kruskal" $ map benchKruskal sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchKruskal :: Int -> Benchmark
benchKruskal n = sizedBench n gen $ nf (kruskal (1, n')) where
    gen = evalR $ replicateM n $ WEdge <$> getRandomR (1, n') <*> getRandomR (1, n') <*> getRandom
    n' = n `div` 4
