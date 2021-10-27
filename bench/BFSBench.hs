module BFSBench where

import Criterion

import BFS ( bfs )
import Util ( evalR, randConnectedGraph, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "BFS"
    [ -- bfs on a graph with n edges and n/4 vertices
      bgroup "bfs" $ map benchBFS sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchBFS :: Int -> Benchmark
benchBFS n = sizedBench n gen $ \g -> nf (bfs g) [1] where
    gen = evalR $ randConnectedGraph (n `div` 4) n
