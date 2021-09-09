module PQTreeBench where

import Criterion
import PQTree
import Util

benchmark :: Benchmark
benchmark = bgroup "PQTree"
    [ -- Build a PQ-tree of size n
      bgroup "buildPQ" $ map benchBuildPQ sizes

      -- Reduce a PQ-tree of size n with n - 1 reductions of pairs of adjacent elements
    , bgroup "reduceAllPQ" $ map benchReduceAllPQ sizes
    ]

sizes :: [Int]
sizes = [100, 2000]

benchBuildPQ :: Int -> Benchmark
benchBuildPQ n = sizedBench n gen $ \us -> nf buildPQ us where
    gen = evalR $ shuffle1 [1..n]

benchReduceAllPQ :: Int -> Benchmark
benchReduceAllPQ n = sizedBench n gen $ \ ~(pqt, xss) -> nf (reduceAllPQ xss) pqt where
    gen = evalR $ do
        us <- shuffle1 [1..n]
        frontier <- shuffle1 us
        xss <- shuffle1 $ zipWith (\x y -> [x, y]) frontier (tail frontier)
        return (buildPQ us, xss)
