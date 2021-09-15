module LCABench where

import Data.List

import Criterion

import LCA ( buildLCA, queryLCA )
import Util ( evalR, randIntPairsR, randTree, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "LCA"
    [ -- Build the LCA structure for a tree of size n
      bgroup "buildLCA" $ map benchBuildLCA sizes

      -- n LCA queries on a tree of size n
    ,  bgroup "queryLCA" $ map benchQueryLCA sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchBuildLCA :: Int -> Benchmark
benchBuildLCA n = sizedBench n gen $ nf go where
    gen = evalR $ randTree n
    go ts = buildLCA (1, n) ts

benchQueryLCA :: Int -> Benchmark
benchQueryLCA n = sizedBench n gen $ \ ~(lca, uvs) -> whnf (go lca) uvs where
    gen = evalR $ (,) <$> (buildLCA (1, n) <$> randTree n) <*> randIntPairsR (1, n) n
    go lca uvs = foldl' (\_ (u, v) -> queryLCA u v lca `seq` ()) () uvs
