module FenwickBench where

import Data.List
import Data.Monoid

import Criterion

import Fenwick
import Util

benchmark :: Benchmark
benchmark = bgroup "Fenwick" [
      -- n updates on a Fenwick tree of size n
        bgroup "updateF" $ map benchUpdateF sizes

      -- n queries on a Fenwick tree of size n
      , bgroup "queryF" $ map benchQueryF sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchUpdateF :: Int -> Benchmark
benchUpdateF n = env (gen n) $ \ ~(ft, us) -> bench (show n) $ nf (go us) ft
    where
        gen n = return (genF n, zip (randIntsR (1, n) n) (Sum <$> randInts n))
        go us ft = foldl' (\ft (i, x) -> updateF i x ft) ft us

benchQueryF :: Int -> Benchmark
benchQueryF n = env (gen n) $ \ ~(ft, qs) -> bench (show n) $ nf (go qs) ft
    where
        gen n = return (genF n, randIntsR (1, n) n)
        go qs ft = foldl' (\_ i -> queryF i ft `seq` ()) () qs

genF :: Int -> FTree (Sum Int)
genF n = buildF (1, n)
