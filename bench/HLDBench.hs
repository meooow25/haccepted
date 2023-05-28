{-# LANGUAGE BangPatterns #-}
module HLDBench where

import Data.List

import Criterion

import HLD ( buildHLD, lcaHLD, pathHLD )
import Util ( evalR, randIntPairsR, randTree, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "HLD"
    [ -- Build the HLD structure for a tree of size n
      bgroup "buildHLD" $ map benchBuildHLD sizes

      -- n path queries on a tree of size n
    , bgroup "pathHLD" $ map benchPathHLD sizes

      -- n LCA queries on a tree of size n
    , bgroup "lcaHLD" $ map benchLCAHLD sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchBuildHLD :: Int -> Benchmark
benchBuildHLD n = sizedBench n gen $ whnf (buildHLD (1,n)) where
    gen = evalR $ randTree n

benchPathHLD :: Int -> Benchmark
benchPathHLD n = sizedBench n gen $ \(hld, qs) -> whnf (go hld) qs where
    gen = evalR $ (,) <$> (buildHLD (1,n) <$> randTree n) <*> randIntPairsR (1,n) n
    -- foldl' instead of just rnf to test fusion of pathHLD
    go hld = foldl' (\_ (u,v) -> foldl' (\_ (!_, !_) -> ()) () (pathHLD hld u v)) ()

benchLCAHLD :: Int -> Benchmark
benchLCAHLD n = sizedBench n gen $ \(hld, qs) -> whnf (go hld) qs where
    gen = evalR $ (,) <$> (buildHLD (1,n) <$> randTree n) <*> randIntPairsR (1,n) n
    go hld = foldl' (\_ (u,v) -> lcaHLD hld u v `seq` ()) ()
