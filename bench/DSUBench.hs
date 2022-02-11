{-# LANGUAGE FlexibleContexts #-}
module DSUBench where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Array.IO

import Criterion

import ArrayNFData ()
import DSU ( newD, sameSetD, unionD )
import Util ( RandStd, evalR, sizedBenchIO )

benchmark :: Benchmark
benchmark = bgroup "DSU"
    [ -- Run sameSet and union n times on a DSU of size n
      bgroup "sameSetD and unionD" $ map benchSameSetDUnionD sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchSameSetDUnionD :: Int -> Benchmark
benchSameSetDUnionD n = sizedBenchIO n gen $ \ ~(dsu, ops) -> whnfIO (go dsu ops) where
    gen = do
        dsu <- newD (1, n) :: IO (IOUArray Int Int)
        let ops = evalR $ replicateM n (randOp n)
        pure (dsu, ops)
    go dsu ops = mapM_ f ops where
        f (SameSet i j) = void $ sameSetD dsu i j
        f (Union   i j) = void $ unionD   dsu i j

data Op = SameSet !Int !Int | Union !Int !Int

instance NFData Op where
    rnf = rwhnf

randOp :: Int -> RandStd Op
randOp n = do
    isSameSet <- getRandom
    (if isSameSet then SameSet else Union) <$> getRandomR (1, n) <*> getRandomR (1, n)
