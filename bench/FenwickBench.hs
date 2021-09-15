module FenwickBench where

import Data.List
import Data.Monoid

import Criterion

import Fenwick ( FTree, buildF, fromListF, queryF, updateF )
import Util ( randInts, randIntsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Fenwick"
    [ -- Build a Fenwick tree from a list of size n
      bgroup "fromListF" $ map benchFromListF sizes

      -- n updates on a Fenwick tree of size n
    , bgroup "updateF" $ map benchUpdateF sizes

      -- n queries on a Fenwick tree of size n
    , bgroup "queryF" $ map benchQueryF sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchFromListF :: Int -> Benchmark
benchFromListF n = sizedBench n gen $ nf (fromListF (1, n)) where
    gen = Sum <$> randInts n

benchUpdateF :: Int -> Benchmark
benchUpdateF n = sizedBench n gen $ \ ~(ft, us) -> nf (go ft) us where
    gen = (buildF (1, n), zip (randIntsR (1, n) n) (Sum <$> randInts n))
    go ft us = foldl' (\ft (i, x) -> updateF i x ft) ft us

benchQueryF :: Int -> Benchmark
benchQueryF n = sizedBench n gen $ \ ~(ft, qs) -> nf (go ft) qs where
    gen = (buildF (1, n) :: FTree (Sum Int), randIntsR (1, n) n)
    go ft qs = foldl' (\_ i -> queryF i ft `seq` ()) () qs
