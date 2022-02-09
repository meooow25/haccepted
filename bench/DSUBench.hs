{-# LANGUAGE FlexibleContexts #-}
module DSUBench where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Array.IO

import Criterion

import ArrayNFData ()
import DSU ( findD, newD, unionD )
import Util ( RandStd, evalR, sizedBenchIO )

benchmark :: Benchmark
benchmark = bgroup "DSU"
    [ -- Run union and find n times on a DSU of size n
      bgroup "findD and unionD" $ map benchfindDUnionD sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchfindDUnionD :: Int -> Benchmark
benchfindDUnionD n = sizedBenchIO n gen $ \ ~(dsu, ops) -> whnfIO (go dsu ops) where
    gen = do
        dsu <- newD (1, n) :: IO (IOUArray Int Int)
        let ops = evalR $ replicateM n (randOp n)
        pure (dsu, ops)
    go dsu ops = mapM_ f ops where
        f (Find  i)   = void $ findD dsu i
        f (Union i j) = void $ unionD dsu i j

data Op = Find !Int | Union !Int !Int

instance NFData Op where
    rnf = rwhnf

randOp :: Int -> RandStd Op
randOp n = do
    isFind <- getRandom
    if isFind
        then Find  <$> getRandomR (1, n)
        else Union <$> getRandomR (1, n) <*> getRandomR (1, n)
