module KMPBench where

import qualified Data.ByteString.Char8 as C

import Criterion

import KMP ( failFuncBS )
import Util ( evalR, randASCIIString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "KMP"
    [ -- Generate the KMP failure function for a string of size n
      bgroup "failFuncBS" $ map benchFailFuncBS sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchFailFuncBS :: Int -> Benchmark
benchFailFuncBS n = sizedBench n gen $ whnf failFuncBS where
    gen = C.pack $ evalR $ randASCIIString n
