module FenwickBench where

import Data.List
import Data.Monoid

import Criterion

import Fenwick ( FTree, emptyF, foldPrefixF, fromListF, mappendF )
import Util ( evalR, randInts, randIntsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Fenwick"
    [ -- Build a Fenwick tree from a list of size n
      bgroup "fromListF" $ map benchFromListF sizes

      -- n updates on a Fenwick tree of size n
    , bgroup "mappendF" $ map benchMappendF sizes

      -- n queries on a Fenwick tree of size n
    , bgroup "foldPrefixF" $ map benchFoldPrefixF sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchFromListF :: Int -> Benchmark
benchFromListF n = sizedBench n gen $ nf $ fromListF (1, n) where
    gen = evalR $ map Sum <$> randInts n

benchMappendF :: Int -> Benchmark
benchMappendF n = sizedBench n gen $ \ ~(ft, us) -> nf (go ft) us where
    gen = (emptyF (1, n), evalR $ zip <$> randIntsR (1, n) n <*> (map Sum <$> randInts n))
    go ft us = foldl' (\ft (i, x) -> mappendF x i ft) ft us

benchFoldPrefixF :: Int -> Benchmark
benchFoldPrefixF n = sizedBench n gen $ \ ~(ft, qs) -> whnf (go ft) qs where
    gen = (emptyF (1, n) :: FTree (Sum Int), evalR $ randIntsR (1, n) n)
    go ft qs = foldl' (\_ i -> foldPrefixF i ft `seq` ()) () qs
