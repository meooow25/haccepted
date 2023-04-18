module SegTreeBench where

import Data.List
import Data.Monoid

import Criterion

import SegTree ( adjustST, emptyST, foldRangeST, fromListST )
import Util ( evalR, randInts, randIntsR, randSortedIntPairsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SegTree"
    [ -- Build a segment tree from a list of size n
      bgroup "fromListST" $ map benchfromListST sizes

      -- n updates on a segment tree of size n
    , bgroup "adjustST" $ map benchAdjustST sizes

      -- n queries on a segment tree of size n
    , bgroup "foldRangeST" $ map benchFoldRangeST sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchfromListST :: Int -> Benchmark
benchfromListST n = sizedBench n gen $ nf $ fromListST (1, n) where
    gen = evalR $ map Sum <$> randInts n

benchAdjustST :: Int -> Benchmark
benchAdjustST n = sizedBench n gen $ \(st, us) -> nf (go st) us where
    gen = (emptyST (1, n), evalR $ zip <$> randIntsR (1, n) n <*> (map Sum <$> randInts n))
    go = foldl' (\st (i, x) -> adjustST (const x) i st)

benchFoldRangeST :: Int -> Benchmark
benchFoldRangeST n = sizedBench n gen $ \(st, qs) -> whnf (go st) qs where
    gen = evalR $ (,) <$>
                  (fromListST (1, n) . map Sum <$> randIntsR (1,n) n) <*>
                  randSortedIntPairsR (1, n) n
    go st = foldl' (\_ (i, j) -> foldRangeST i j st `seq` ()) ()
