{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SegTreeLazyBench where

import Control.DeepSeq
import Data.List
import Data.Monoid

import Criterion

import SegTreeLazy
    ( LazySegTree
    , adjustLST
    , emptyLST
    , foldRangeLST
    , fromListLST
    , updateRangeLST
    )
import Misc ( Action(..) )
import Util ( evalR, randInts, randIntsR, randSortedIntPairsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SegTreeLazy"
    [ -- Build a segment tree from a list of size n
      bgroup "fromListLST" $ map benchfromListLST sizes

      -- n updates on a segment tree of size n
    , bgroup "adjustLST" $ map benchAdjustLST sizes

      -- n range updates on a segment tree of size n
    , bgroup "updateRangeLST" $ map benchUpdateRangeLST sizes

      -- n queries on a segment tree of size n
    , bgroup "foldRangeLST" $ map benchFoldRangeLST sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

data SumLen = SumLen !Int !Int deriving Show

instance NFData SumLen where
    rnf (SumLen s l) = rnf s `seq` rnf l

instance Semigroup SumLen where
    SumLen s1 l1 <> SumLen s2 l2 = SumLen (s1 + s2) (l1 + l2)

instance Monoid SumLen where
    mempty = SumLen 0 0

-- Can add a value to all elements in a range
type RangeAddSegTree = LazySegTree (Sum Int) SumLen

instance Action (Sum Int) SumLen where
    act (SumLen s l) (Sum u) = SumLen (s + u * l) l

benchfromListLST :: Int -> Benchmark
benchfromListLST n = sizedBench n gen $ nf $ go (1, n) where
    gen = evalR $ map (\x -> SumLen x 1) <$> randInts n
    go :: (Int, Int) -> [SumLen] -> RangeAddSegTree
    go = fromListLST

benchAdjustLST :: Int -> Benchmark
benchAdjustLST n = sizedBench n gen $ \(st, us) -> nf (go st) us where
    gen =
        ( emptyLST (1, n) :: RangeAddSegTree
        , evalR $ zip <$> randIntsR (1, n) n <*> randInts n
        )
    addToSingle x (SumLen s _) = SumLen (s + x) 1
    go :: RangeAddSegTree -> [(Int, Int)] -> RangeAddSegTree
    go = foldl' (\st (i, x) -> adjustLST (addToSingle x) i st)

benchUpdateRangeLST :: Int -> Benchmark
benchUpdateRangeLST n = sizedBench n gen $ \(st, qs) -> nf (go st) qs where
    gen =
        ( emptyLST (1, n) :: RangeAddSegTree
        , evalR $ zip <$> randSortedIntPairsR (1, n) n <*> (map Sum <$> randInts n)
        )
    go = foldl' (\st ((i, j), u) -> updateRangeLST u i j st)

benchFoldRangeLST :: Int -> Benchmark
benchFoldRangeLST n = sizedBench n gen $ \(st, qs) -> whnf (go st) qs where
    gen = evalR $ (,) <$>
                  (fromListLST (1, n) . map (\x -> SumLen x 1) <$> randIntsR (1,n) n) <*>
                  randSortedIntPairsR (1, n) n
    go :: RangeAddSegTree -> [(Int, Int)] -> ()
    go st = foldl' (\_ (i, j) -> foldRangeLST i j st `seq` ()) ()
