{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module SuffixArrayBench where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C

import Criterion

import ArrayNFData ()
import SuffixArray ( buildSufA, buildSufAL )
import Util ( evalR, randInts, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SuffixArray"
    [ -- Build a suffix array and LCP array from an a-z string of length n.
      bgroup "buildSufA" $ map benchBuild sizes
    
    , -- Build a suffix array and LCP array from a string of length n.
      bgroup "buildSufAL" $ map benchBuildL sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchBuild :: Int -> Benchmark
benchBuild n = sizedBench n gen $ nf go where
    gen = C.pack $ evalR $ randLowerCaseString n
    go s = buildSufA 128 (C.length s) (fromEnum . C.index s)

benchBuildL :: Int -> Benchmark
benchBuildL n = sizedBench n gen $ nf go where
    gen = listArray @UArray (0, n-1) $ evalR $ randInts n
    go a = buildSufAL (snd $ bounds a) (a!)
