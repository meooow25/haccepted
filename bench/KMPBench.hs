module KMPBench where

import qualified Data.ByteString.Char8 as C

import Criterion

import KMP ( prefixFuncBS )
import Util ( evalR, randASCIIString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "KMP"
    [ -- Generate the KMP prefix function for a string of size n
      bgroup "prefixFuncBS" $ map benchPrefixFuncBS sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchPrefixFuncBS :: Int -> Benchmark
benchPrefixFuncBS n = sizedBench n gen $ whnf prefixFuncBS where
    gen = C.pack $ evalR $ randASCIIString n
