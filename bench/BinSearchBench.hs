module BinSearchBench where

import Data.Array
import Data.List

import Criterion

import BinSearch ( binSearch, binSearchA )
import Util ( evalR, randIntsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "BinSearch"
    [ -- n searches on a range of size n
      bgroup "binSearch" $ map benchBinSearch sizes

      -- n searches on an array of size n
    , bgroup "binSearchA" $ map benchBinSearchA sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchBinSearch :: Int -> Benchmark
benchBinSearch n = sizedBench n gen $ \qs -> whnf (go n) qs where
    gen = evalR $ randIntsR (1, n) n
    go n qs = foldl' (\_ i -> binSearch (>= i) 1 n `seq` ()) () qs

benchBinSearchA :: Int -> Benchmark
benchBinSearchA n = sizedBench n gen $ \ ~(a, qs) -> whnf (go a) qs where
    gen = (listArray (1, n) [1..n], evalR $ randIntsR (1, n) n)
    go a qs = foldl' (\_ i -> binSearchA (>= i) a `seq` ()) () qs
