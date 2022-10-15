module SuffixArrayBench where

import qualified Data.ByteString.Char8 as C

import Criterion

import ArrayNFData ()
import SuffixArray
import Util ( evalR, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SuffixArray"
    [ -- Build a suffix array from an a-z string of length n.
      bgroup "build" $ map benchBuild sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchBuild :: Int -> Benchmark
benchBuild n = sizedBench n gen $ \s -> nf (buildSufArray (C.length s)) (fromEnum . C.index s) where
    gen = evalR $ C.pack <$> randLowerCaseString n
