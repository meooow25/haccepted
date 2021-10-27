module ZBench where

import qualified Data.ByteString.Char8 as C

import Criterion

import ZFunc ( zFuncBS )
import Util ( evalR, randASCIIString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "ZFunc"
    [ -- Generate the Z-function for a string of size n
      bgroup "zFuncBS" $ map benchZFuncBS sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchZFuncBS :: Int -> Benchmark
benchZFuncBS n = sizedBench n gen $ whnf zFuncBS where
    gen = C.pack $ evalR $ randASCIIString n
