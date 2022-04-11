module DijkstraBench where

import Control.Monad.Random
import Data.Array.Unboxed
import Data.Graph

import Criterion

import Dijkstra ( Weight, dijkstra, dijkstraH )
import LabelledGraph ( LGraph )
import Util ( evalR, sizedBench, randConnectedGraph )

benchmark :: Benchmark
benchmark = bgroup "Dijkstra"
    [ -- Run dijkstra on a graph with n edges and n/5 vertices
      bgroup "dijkstra" $ map (benchDijkstra dijkstra) sizes

      -- Run dijkstraH on a graph with n edges and n/5 vertices
    , bgroup "dijkstraH" $ map (benchDijkstra dijkstraH) sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

type DijkstraF = LGraph Weight -> [Vertex] -> UArray Vertex Weight

benchDijkstra :: DijkstraF -> Int -> Benchmark
benchDijkstra dijkstraf n = sizedBench n gen $ \g -> whnf (dijkstraf g) [1] where
    gen = evalR $
        randConnectedGraph (div n 5) n >>=
            (traverse . traverse) ((<$> getRandomR (0, 10^(9 :: Int))) . flip (,))
