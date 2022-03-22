module DinicBench where

import Control.Monad
import Control.Monad.Random
import Data.Ix

import Criterion

import Dinic ( FlowEdge(..), dinic )
import Util ( evalR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Dinic"
    [ -- Run Dinic's algorithm on a graph with n vertices and 50n edges.
      bgroup "dinic" $ map benchDinic sizes

      -- Run Dinic's algorithm on a bipartite graph with n vertices and 2n edges.
    , bgroup "dinic bipartite" $ map benchDinicBipartite bipartiteSizes
    ]

sizes :: [Int]
sizes = [100, 20000]

benchDinic :: Int -> Benchmark
benchDinic n = sizedBench n gen $ \es -> whnf (dinic (1, n) es 1) n where
    gen = evalR $ replicateM (n * 50) $
        FlowEdge <$> getRandomR (1, n) <*> getRandomR (1, n) <*> getRandomR (1, 10 ^ (9 :: Int))

bipartiteSizes :: [Int]
bipartiteSizes = [100, 10000, 100000]

benchDinicBipartite :: Int -> Benchmark
benchDinicBipartite n = sizedBench n gen $ \es -> whnf (dinic (1, n + 2) es (n + 1)) (n + 2) where
    gen = evalR $ do
        let lb = (1, div n 2)
            rb = (div n 2 + 1, n)
            les = [FlowEdge (n + 1) i 1 | i <- range lb]
            res = [FlowEdge i (n + 2) 1 | i <- range rb]
        es <- replicateM (2 * n) $ FlowEdge <$> getRandomR lb <*> getRandomR rb <*> pure 1
        pure $ les ++ es ++ res
