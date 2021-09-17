module PQTreeBench where

import Criterion

import PQTree ( buildPQ, reduceAllPQ )
import Util ( evalR, shuffle, sizedBench )

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
benchBuildPQ n = sizedBench n gen $ nf buildPQ where
    gen = evalR $ shuffle [1..n]

benchReduceAllPQ :: Int -> Benchmark
benchReduceAllPQ n = sizedBench n gen $ \ ~(pqt, xss) -> nf (reduceAllPQ xss) pqt where
    gen = evalR $ do
        us <- shuffle [1..n]
        frontier <- shuffle us
        xss <- shuffle $ zipWith (\x y -> [x, y]) frontier (tail frontier)
        pure (buildPQ us, xss)
