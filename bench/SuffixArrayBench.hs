{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module SuffixArrayBench where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C

import Criterion

import ArrayNFData ()
import SuffixArray ( buildSufArray, buildSufArrayL )
import Util ( evalR, randInts, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SuffixArray"
    [ -- Build a suffix array from an a-z string of length n.
      bgroup "build" $ map benchBuild sizes
    
    , -- Build a suffix array from a string of length n.
      bgroup "build large alphabet" $ map benchBuildL sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchBuild :: Int -> Benchmark
benchBuild n = sizedBench n gen $ nf go where
    gen = C.pack $ evalR $ randLowerCaseString n
    go s = buildSufArray 128 (C.length s) (fromEnum . C.index s)

benchBuildL :: Int -> Benchmark
benchBuildL n = sizedBench n gen $ nf go where
    gen = listArray @UArray (0, n-1) $ evalR $ randInts n
    go a = buildSufArrayL (snd $ bounds a) (a!)
