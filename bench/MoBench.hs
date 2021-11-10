module MoBench where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef

import Criterion

import Mo ( MoQuery(..), Tag, runMo, sqrtSize )
import Util ( evalR, randIntsR, randSortedIntPairsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Mo"
    [ -- Run Mo's algorithm with n queries on a sequence of size n
      bgroup "runMo" $ map benchRunMo sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchRunMo :: Int -> Benchmark
benchRunMo n = sizedBench n gen $ \ ~(xa, qrys) -> nf (countDistinct xa) qrys where
    gen = evalR $ do
        lrs <- randSortedIntPairsR (1, n) n
        xs <- randIntsR (1, n) n
        let xa = listArray (1, n) xs
            qrys = [MoQuery l r i | (i, (l, r)) <- zip [1..] lrs]
        pure (xa, qrys)

-- Count number of distinct elements in a range, a classic Mo problem.
countDistinct :: UArray Int Int -> [MoQuery] -> [(Tag, Int)]
countDistinct xa qrys = runST $ do
    cnts <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
    cur <- newSTRef 0
    let add i = do
            c <- readArray cnts (xa!i)
            when (c == 0) $ modifySTRef' cur (+1)
            writeArray cnts (xa!i) $ c + 1
        rem i = do
            c <- readArray cnts (xa!i)
            when (c == 1) $ modifySTRef' cur (subtract 1)
            writeArray cnts (xa!i) $ c - 1
        ans = readSTRef cur
    runMo (sqrtSize n) add rem ans qrys
  where
    (1, n) = bounds xa

-- A UArray is strict in its elements
instance NFData (UArray i e) where
    rnf = rwhnf
