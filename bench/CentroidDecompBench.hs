module CentroidDecompBench where

import Criterion

import CentroidDecomp ( centroidDecompose )
import Util ( evalR, randTree, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "CentroidDecomp"
    [ -- Centroid decompose a tree of size n
      bgroup "centroidDecompose" $ map benchCentroidDecompose sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchCentroidDecompose :: Int -> Benchmark
benchCentroidDecompose n = sizedBench n gen $ nf centroidDecompose where
    gen = evalR $ randTree n
