{-# LANGUAGE FlexibleInstances #-}
module SegTreeMutBench where

import Control.DeepSeq
import Control.Monad
import Data.Foldable
import Data.Monoid

import Criterion

import SegTreeMut
    ( SegTreeMut
    , adjustSTM
    , binSearchSTM
    , emptySTM
    , foldRangeSTM
    , fromListSTM
    )
import Array ( IOUArr )
import Util ( evalR, randInts, randIntsR, randSortedIntPairsR, sizedBench, sizedBenchIO )

benchmark :: Benchmark
benchmark = bgroup "SegTreeMut"
    [ -- Build a segment tree from a list of size n
      bgroup "fromListSTM" $ map benchfromListSTM sizes

      -- n updates on a segment tree of size n
    , bgroup "adjustSTM" $ map benchAdjustSTM sizes

      -- n queries on a segment tree of size n
    , bgroup "foldRangeSTM" $ map benchFoldRangeSTM sizes

      -- n binary searches on a segment tree of size n
    , bgroup "binSearchSTM" $ map benchBinSearchSTM sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

type SumSegTree = SegTreeMut IOUArr (Sum Int)

-- The array in SumSegTree is unboxed
instance NFData SumSegTree where
    rnf = rwhnf

benchfromListSTM :: Int -> Benchmark
benchfromListSTM n = sizedBench n gen $ whnfIO . go (1, n) where
    gen = evalR $ map Sum <$> randInts n
    go :: (Int, Int) -> [Sum Int] -> IO SumSegTree
    go = fromListSTM

benchAdjustSTM :: Int -> Benchmark
benchAdjustSTM n = sizedBenchIO n gen $ \(st, us) -> whnfIO (go st us) where
    gen = (,) <$> emptySTM (1, n) <*> pure (evalR $ zip <$> randIntsR (1, n) n <*> randInts n)
    go :: SumSegTree -> [(Int, Int)] -> IO ()
    go st = traverse_ (\(i,x) -> adjustSTM st i (const $ Sum x))

benchFoldRangeSTM :: Int -> Benchmark
benchFoldRangeSTM n = sizedBenchIO n gen $ \(st, qs) -> whnfIO (go st qs) where
    gen = do
        let (xs, qrys) = evalR $ (,) <$> randIntsR (1,n) n <*> randSortedIntPairsR (1,n) n
        st <- fromListSTM (1,n) $ map Sum xs
        pure (st, qrys)
    go :: SumSegTree -> [(Int, Int)] -> IO ()
    go st = traverse_ (\(i,j) -> id <$!> foldRangeSTM st i j)

benchBinSearchSTM :: Int -> Benchmark
benchBinSearchSTM n = sizedBenchIO n gen $ \(st, qs) -> whnfIO (go st qs) where
    gen = do
        let (xs, qrys) = evalR $ (,) <$>
                                 randIntsR (1,n) n <*>
                                 (zip <$> randSortedIntPairsR (1,n) n <*> randInts n)
        st <- fromListSTM (1,n) $ map Sum xs
        pure (st, qrys)
    go :: SumSegTree -> [((Int, Int), Int)] -> IO ()
    go st = traverse_ (\((i,j),x) -> id <$!> binSearchSTM st i j ((>=x) . getSum))
