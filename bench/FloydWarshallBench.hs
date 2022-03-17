module FloydWarshallBench where

import Data.Array.IO
import Data.Graph

import Criterion

import ArrayNFData ()
import FloydWarshall ( Weight, floydWarshall )
import Util ( evalR, randIntsR, sizedBenchIO )

benchmark :: Benchmark
benchmark = bgroup "Floyd-Warshall"
    [ -- Run floydWarshall on a graph with n vertices
      bgroup "floydWarshall" $ map benchFloydWarshall sizes
    ]

sizes :: [Int]
sizes = [100, 500]

benchFloydWarshall :: Int -> Benchmark
benchFloydWarshall n = sizedBenchIO n gen $ \d -> whnfIO (floydWarshall d) where
    gen = newListArray ((1, 1), (n, n)) ws :: IO (IOUArray (Vertex, Vertex) Weight) where
        ws = evalR $ randIntsR (1, 10^(9 :: Int)) (n * n)
