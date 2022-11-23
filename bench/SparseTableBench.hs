module SparseTableBench where

import Data.Semigroup
import Data.List

import Criterion

import SparseTable ( fromListSP, fromListISP, fromListUSP, fromListIUSP )
import Util ( evalR, randInts, randSortedIntPairsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SparseTable"
    [ -- Build a sparse table of size n
      bgroup "fromListSP Sum" $ map benchFromListSP sizes

    , -- Build a sparse table of size n
      bgroup "fromListISP Min" $ map benchFromListISP sizes

      -- n queries on a sparse table of size n
    , bgroup "queries SP Sum" $ map benchQuerySP sizes

      -- n queries on a sparse table of size n (idempotent)
    , bgroup "queries ISP Min" $ map benchQueryISP sizes

      -- Build an unboxed sparse table of size n
    , bgroup "fromListUSP (+)" $ map benchFromListUSP sizes

      -- Build an unboxed sparse table of size n
    , bgroup "fromListIUSP min" $ map benchFromListIUSP sizes

      -- n queries on an unboxed sparse table of size n
    , bgroup "queries USP (+)" $ map benchQueryUSP sizes

      -- n queries on an unboxed sparse table of size n (idempotent)
    , bgroup "queries IUSP min" $ map benchQueryIUSP sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchFromListSP :: Int -> Benchmark
benchFromListSP n = sizedBench n gen $ whnf go where
    gen = map Sum $ evalR $ randInts n
    go = fromListSP (1, n)

benchFromListISP :: Int -> Benchmark
benchFromListISP n = sizedBench n gen $ whnf go where
    gen = map Min $ evalR $ randInts n
    go = fromListISP (1, n)

benchQuerySP :: Int -> Benchmark
benchQuerySP n = sizedBench n gen $ \(qf, qs) -> whnf (go qf) qs where
    gen = evalR $ (,) <$> (fromListSP (1, n) . map Sum <$> randInts n)
                      <*> randSortedIntPairsR (1, n) n
    go qf = foldl' (\_ (l, r) -> qf l r `seq` ()) ()

benchQueryISP :: Int -> Benchmark
benchQueryISP n = sizedBench n gen $ \(qf, qs) -> whnf (go qf) qs where
    gen = evalR $ (,) <$> (fromListISP (1, n) . map Min <$> randInts n)
                      <*> randSortedIntPairsR (1, n) n
    go qf = foldl' (\_ (l, r) -> qf l r `seq` ()) ()

benchFromListUSP :: Int -> Benchmark
benchFromListUSP n = sizedBench n gen $ whnf go where
    gen = evalR $ randInts n
    go = fromListUSP (+) (1, n)

benchFromListIUSP :: Int -> Benchmark
benchFromListIUSP n = sizedBench n gen $ whnf go where
    gen = evalR $ randInts n
    go = fromListIUSP min (1, n)

benchQueryUSP :: Int -> Benchmark
benchQueryUSP n = sizedBench n gen $ \(qf, qs) -> whnf (go qf) qs where
    gen = evalR $ (,) <$> (fromListUSP (+) (1, n) <$> randInts n)
                      <*> randSortedIntPairsR (1, n) n
    go qf = foldl' (\_ (l, r) -> qf l r `seq` ()) ()

benchQueryIUSP :: Int -> Benchmark
benchQueryIUSP n = sizedBench n gen $ \(qf, qs) -> whnf (go qf) qs where
    gen = evalR $ (,) <$> (fromListIUSP min (1, n) <$> randInts n)
                      <*> randSortedIntPairsR (1, n) n
    go qf = foldl' (\_ (l, r) -> qf l r `seq` ()) ()
