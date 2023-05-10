module RerootFoldBench where

import Criterion

import RerootFold ( foldReroot )
import Util ( evalR, randTree, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "RerootFold"
    [ -- foldReroot a tree of size n
      bgroup "foldReroot" $ map benchCentroidDecompose sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchCentroidDecompose :: Int -> Benchmark
benchCentroidDecompose n = sizedBench n gen $ nf go where
    gen = evalR $ randTree n
    go = foldReroot (const (max 1)) (+) (0 :: Int) -- depth of the tree
