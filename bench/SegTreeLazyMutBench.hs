{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module SegTreeLazyMutBench where

import Control.DeepSeq
import Control.Monad
import Data.Array.IO
import Data.Foldable
import Data.Monoid

import Criterion

import SegTreeLazyMut
    ( LazySegTreeMut
    , adjustLSTM
    , binSearchLSTM
    , emptyLSTM
    , foldRangeLSTM
    , fromListLSTM
    , updateRangeLSTM
    )
import Array ( Arr, Unbox(..), IOUArr )
import Array2 ( Arr2 )
import SegTreeLazyBench ( SumLen(..) )
import Util ( evalR, randInts, randIntsR, randSortedIntPairsR, sizedBench, sizedBenchIO )

benchmark :: Benchmark
benchmark = bgroup "SegTreeLazyMut"
    [ -- Build a segment tree from a list of size n
      bgroup "fromListLSTM" $ map benchfromListLSTM sizes

      -- n updates on a segment tree of size n
    , bgroup "adjustLSTM" $ map benchAdjustLSTM sizes

      -- n range updates on a segment tree of size n
    , bgroup "updateRangeLSTM" $ map benchUpdateRangeLSTM sizes

      -- n queries on a segment tree of size n
    , bgroup "foldRangeLSTM" $ map benchFoldRangeLSTM sizes

      -- n binary searches on a segment tree of size n
    , bgroup "binSearchLSTM" $ map benchBinSearchLSTM sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

-- Can add a value to all elements in a range
type RangeAddSegTree = LazySegTreeMut IOUArr (Arr (Arr2 IOUArray IOUArray)) (Sum Int) SumLen

instance Unbox (Sum a) where
    type Unboxed (Sum a) = a

instance Unbox SumLen where
    type Unboxed SumLen = (Int, Int)
    toU (SumLen s n) = (s, n)
    frU (s, n) = SumLen s n

-- Both arrays in RangeAddSegTree are unboxed
instance NFData RangeAddSegTree where
    rnf = rwhnf

benchfromListLSTM :: Int -> Benchmark
benchfromListLSTM n = sizedBench n gen $ whnfIO . go (1, n) where
    gen = evalR $ map (\x -> SumLen x 1) <$> randInts n
    go :: (Int, Int) -> [SumLen] -> IO RangeAddSegTree
    go = fromListLSTM

benchAdjustLSTM :: Int -> Benchmark
benchAdjustLSTM n = sizedBenchIO n gen $ \ ~(st, us) -> whnfIO (go st us) where
    gen = (,) <$> emptyLSTM (1, n) <*> pure (evalR $ zip <$> randIntsR (1, n) n <*> randInts n)
    addToSingle x (SumLen s _) = SumLen (s + x) 1
    go :: RangeAddSegTree -> [(Int, Int)] -> IO ()
    go st = traverse_ (\(i,x) -> adjustLSTM st i (addToSingle x))

benchUpdateRangeLSTM :: Int -> Benchmark
benchUpdateRangeLSTM n = sizedBenchIO n gen $ \ ~(st, qs) -> whnfIO (go st qs) where
    gen = (,) <$>
          emptyLSTM (1, n) <*>
          pure (evalR $ zip <$> randSortedIntPairsR (1, n) n <*> (map Sum <$> randInts n))
    go :: RangeAddSegTree -> [((Int, Int), Sum Int)] -> IO ()
    go st = traverse_ (\((i,j), u) -> updateRangeLSTM st i j u)

benchFoldRangeLSTM :: Int -> Benchmark
benchFoldRangeLSTM n = sizedBenchIO n gen $ \ ~(st, qs) -> whnfIO (go st qs) where
    gen = (,) <$> emptyLSTM (1, n) <*> pure (evalR $ randSortedIntPairsR (1, n) n)
    go :: RangeAddSegTree -> [(Int, Int)] -> IO ()
    go st = traverse_ (\(i,j) -> id <$!> foldRangeLSTM st i j)

benchBinSearchLSTM :: Int -> Benchmark
benchBinSearchLSTM n = sizedBenchIO n gen $ \ ~(st, qs) -> whnfIO (go st qs) where
    gen = do
        let (xs, qrys) = evalR $ (,) <$>
                                 randIntsR (1,n) n <*>
                                 (zip <$> randSortedIntPairsR (1,n) n <*> randInts n)
        st <- fromListLSTM (1,n) $ map (\x -> SumLen x 1) xs
        pure (st, qrys)
    go :: RangeAddSegTree -> [((Int, Int), Int)] -> IO ()
    go st = traverse_ (\((i,j),x) -> id <$!> binSearchLSTM st i j (\(SumLen s _) -> s >= x))
