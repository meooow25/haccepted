module Util where

import Control.DeepSeq
import System.Random

import Criterion

seed :: Int
seed = 42

randIntsR :: (Int, Int) -> Int -> [Int]
randIntsR bnds n = take n $ randomRs bnds $ mkStdGen seed

randInts :: Int -> [Int]
randInts n = take n $ randoms $ mkStdGen seed

randSortedIntPairsR :: (Int, Int) -> Int -> [(Int, Int)]
randSortedIntPairsR bnds n =
    uncurry (zipWith (\x y -> (min x y, max x y))) $ splitAt n $ randIntsR bnds $ 2 * n

{-# INLINE sizedBench #-}
sizedBench :: NFData env => Int -> env -> (env -> Benchmarkable) -> Benchmark
sizedBench n e b = env (return e) $ bench (show n) . b
