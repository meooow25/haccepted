module BinSearchBench where

import Data.Array
import Data.List

import Criterion

import BinSearch
import Util

benchmark :: Benchmark
benchmark = bgroup "BinSearch" [
        -- n searches on a range of size n
        bgroup "binSearch" $ map benchBinSeaarch sizes

        -- n searches on an array of size n
      , bgroup "binSearchA" $ map benchBinSeaarchA sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchBinSeaarch :: Int -> Benchmark
benchBinSeaarch n = env (gen n) $ \qs -> bench (show n) $ whnf (go qs) n
    where
        gen n = return $ randIntsR (1, n) n
        go qs n = foldl' (\_ i -> binSearch (>= i) 1 n `seq` ()) () qs

benchBinSeaarchA :: Int -> Benchmark
benchBinSeaarchA n = env (gen n) $ \ ~(qs, a) -> bench (show n) $ whnf (go qs) a
    where
        gen n = return (randIntsR (1, n) n, listArray (1, n) [1..n])
        go qs a = foldl' (\_ i -> binSearchA (>= i) a `seq` ()) () qs
