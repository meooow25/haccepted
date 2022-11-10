module FenwickBench where

import Control.DeepSeq
import Data.List
import Data.Monoid

import Criterion

import Fenwick ( FTree, binSearchF, emptyF, foldPrefixF, fromListF, mappendF )
import Util ( RandStd, evalR, randInts, randIntsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Fenwick"
    [ -- Build a Fenwick tree from a list of size n
      bgroup "fromListF" $ map benchFromListF sizes

      -- n updates on an empty Fenwick tree of size n
    , bgroup "mappendF" $ map benchMappendF sizes

      -- n queries on a Fenwick tree of size n
    , bgroup "foldPrefixF" $ map benchFoldPrefixF sizes

      -- n binary searches on a Fenwick tree of size n
    , bgroup "binSearchF" $ map benchBinSearchF sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchFromListF :: Int -> Benchmark
benchFromListF n = sizedBench n gen $ nf $ fromListF (1, n) where
    gen = evalR $ map Sum <$> randInts n

benchMappendF :: Int -> Benchmark
benchMappendF n = sizedBench n gen $ \(ft, us) -> nf (go ft) us where
    gen = (emptyF (1, n), evalR $ zip <$> randIntsR (1, n) n <*> (map Sum <$> randInts n))
    go = foldl' (\ft (i, x) -> mappendF i x ft)

benchFoldPrefixF :: Int -> Benchmark
benchFoldPrefixF n = sizedBench n gen $ \(ft, qs) -> whnf (go ft) qs where
    gen = evalR $ (,) <$> genFTree n <*> randIntsR (1, n) n
    go ft = foldl' (\_ i -> foldPrefixF i ft `seq` ()) ()

benchBinSearchF :: Int -> Benchmark
benchBinSearchF n = sizedBench n gen $ \(ft, qs) -> whnf (go ft) qs where
    gen = evalR $ (,) <$> genFTree n <*> (map Sum <$> randIntsR (1, n * n) n)
    go ft qs = foldl' (\_ x -> rnf $ binSearchF (>=x) ft) () qs

genFTree :: Int -> RandStd (FTree (Sum Int))
genFTree n = fromListF (1, n) . map Sum <$> randIntsR (1, n) n
