module LCABench where

import Control.DeepSeq
import Data.List

import Criterion

import LCA ( buildLCA, query1LCA )
import Util ( randIntPairsR, randForest, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "LCA"
    [ -- Build the LCA structure for a tree of size n
      bgroup "buildLCA" $ map benchBuildLCA sizes

      -- n LCA queries on a tree of size n
    ,  bgroup "query1LCA" $ map benchQuery1LCA sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchBuildLCA :: Int -> Benchmark
benchBuildLCA n = sizedBench n gen $ nf go where
    gen = randForest n
    go ts = buildLCA (1, n) ts

benchQuery1LCA :: Int -> Benchmark
benchQuery1LCA n = sizedBench n gen $ \ ~(lca, uvs) -> whnf (go lca) uvs where
    gen = (buildLCA (1, n) $ randForest n, randIntPairsR (1, n) n)
    go lca uvs = foldl' (\_ (u, v) -> rnf $ query1LCA u v lca) () uvs
