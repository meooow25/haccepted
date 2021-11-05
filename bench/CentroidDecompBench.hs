module CentroidDecompBench where

import Criterion

import CentroidDecomp ( centroidDecompose, centroidDecomposeL )
import Util ( evalR, randLTree, randTree, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "CentroidDecomp"
    [ -- Centroid decompose a tree of size n
      bgroup "centroidDecompose" $ map benchCentroidDecompose sizes
    
      -- Centroid decompose a labelled tree of size n
    , bgroup "centroidDecomposeL" $ map benchCentroidDecomposeL sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchCentroidDecompose :: Int -> Benchmark
benchCentroidDecompose n = sizedBench n gen $ nf centroidDecompose where
    gen = evalR $ randTree n

benchCentroidDecomposeL :: Int -> Benchmark
benchCentroidDecomposeL n = sizedBench n gen $ nf centroidDecomposeL where
    gen = evalR $ randLTree n
