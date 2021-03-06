module SparseTableBench where

import Data.Array
import Data.Monoid
import Data.List

import Criterion

import SparseTable ( SparseTable, fromArraySP, fromListSP, query1SP, querySP )
import Util ( evalR, randInts, randSortedIntPairsR, sizedBench )

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
benchFromArraySP n = sizedBench n gen $ nf fromArraySP where
    gen = evalR $ listArray (1, n) . map Sum <$> randInts n

benchFromListSP :: Int -> Benchmark
benchFromListSP n = sizedBench n gen $ nf $ fromListSP (1, n) where
    gen = evalR $ map Sum <$> randInts n

type QF = Int -> Int -> SparseTable (Sum Int) -> Sum Int

benchQuery :: QF -> Int -> Benchmark
benchQuery query n = sizedBench n gen $ \ ~(sp, qs) -> whnf (go sp) qs where
    gen = evalR $ (,) <$> (fromListSP (1, n) . map Sum <$> randInts n) <*> randSortedIntPairsR (1, n) n
    go sp qs = foldl' (\_ (l, r) -> query l r sp `seq` ()) () qs
