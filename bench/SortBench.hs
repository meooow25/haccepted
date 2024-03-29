{-# LANGUAGE TypeApplications #-}
module SortBench where

import Data.Array.Unboxed

import Criterion

import ArrayNFData ()
import Sort ( sort, sortU, sortUABy, countingSortUA )
import Util ( evalR, randInts, randIntsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Sort"
    [ -- Sort n ints
      bgroup "sort" $ map (benchSort sort) sizes
    
      -- Sort n ints
    , bgroup "sortU" $ map (benchSort sortU) sizes

      -- Sort n ints
    , bgroup "sortUABy" $ map benchSortUABy sizes

      -- Counting sort n ints in [0..255]
    , bgroup "countingSortUA" $ map benchCountingSort sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchSort :: ([Int] -> [Int]) -> Int -> Benchmark
benchSort sortF n = sizedBench n gen $ nf sortF where
    gen = evalR $ randInts n

benchSortUABy :: Int -> Benchmark
benchSortUABy n = sizedBench n gen $ whnf (sortUABy compare) where
    gen = listArray @UArray (1, n) $ evalR $ randInts n

benchCountingSort :: Int -> Benchmark
benchCountingSort n = sizedBench n gen $ whnf (countingSortUA b id) where
    b = 256
    gen = listArray @UArray (1, n) $ evalR $ randIntsR (0, b-1) n
