module SparseTableBench where

import Data.Array
import Data.Monoid
import Data.List

import Criterion

import SparseTable
import Util

benchmark :: Benchmark
benchmark = bgroup "SparseTable"
    [ -- Build a sparse table of size n from an array
      bgroup "fromArraySP" $ map benchFromArraySP sizes

      -- Build a sparse table of size n from a list
    , bgroup "fromListSP" $ map benchFromListSP sizes

      -- n queries on a sparse table of size n
    , bgroup "querySP" $ map (benchQuery querySP) sizes

      -- n queries on a sparse table of size n
    , bgroup "query1SP" $ map (benchQuery query1SP) sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchFromArraySP :: Int -> Benchmark
benchFromArraySP n = sizedBench n gen $ nf go
    where
        gen = listArray (1, n) $ Sum <$> randInts n
        go xa = fromArraySP xa

benchFromListSP :: Int -> Benchmark
benchFromListSP n = sizedBench n gen $ nf go
    where
        gen = Sum <$> randInts n
        go xa = fromListSP (1, n) xa

type QF = Int -> Int -> SparseTable (Sum Int) -> Sum Int

benchQuery :: QF -> Int -> Benchmark
benchQuery query n = sizedBench n gen $ \ ~(sp, qs) -> whnf (go sp) qs
    where
        gen = (fromListSP (1, n) $ Sum <$> randInts n, randSortedIntPairsR (1, n) n)
        go sp qs = foldl' (\_ (l, r) -> query l r sp `seq` ()) () qs
