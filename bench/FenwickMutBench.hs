module FenwickMutBench where

import Data.Foldable
import Data.Monoid

import Criterion

import FenwickMut ( FenwickMut, emptyFM, foldPrefixFM, mappendFM)
import Array ( IOUArr )
import ArrayNFData ()
import Util ( evalR, randInts, randIntsR, sizedBenchIO )

benchmark :: Benchmark
benchmark = bgroup "FenwickMut"
    [ -- n updates on an empty Fenwick tree of size n
      bgroup "mappendFM" $ map benchMappendF sizes

      -- n queries on a Fenwick tree of size n
    , bgroup "foldPrefixFM" $ map benchFoldPrefixF sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchMappendF :: Int -> Benchmark
benchMappendF n = sizedBenchIO n gen $ \(ft, us) -> whnfIO (go ft us) where
    gen = (,) <$>
          emptyFM (1,n) <*>
          pure (evalR $ zip <$> randIntsR (1,n) n <*> (map Sum <$> randInts n))
    go :: FenwickMut IOUArr (Sum Int) -> [(Int, Sum Int)] -> IO ()
    go ft = traverse_ $ uncurry (mappendFM ft)

benchFoldPrefixF :: Int -> Benchmark
benchFoldPrefixF n = sizedBenchIO n gen $ \(ft, qs) -> whnfIO (go ft qs) where
    gen = do
        let (xs, qrys) = evalR $ (,) <$> randIntsR (1,n) n <*> randIntsR (1, n) n
        ft <- emptyFM (1,n)
        for_ (zip [1..] xs) $ \(i,x) -> mappendFM ft i (Sum x)
        pure (ft, qrys)
    go :: FenwickMut IOUArr (Sum Int) -> [Int] -> IO ()
    go ft = traverse_ $ foldPrefixFM ft
