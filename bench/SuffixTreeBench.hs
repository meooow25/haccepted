module SuffixTreeBench where

import Control.Monad
import qualified Data.ByteString.Char8 as C

import Criterion

import SuffixTree
import Util ( evalR, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SuffixTree"
    [ bgroup "build" $ map benchBuild sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchBuild :: Int -> Benchmark
benchBuild n = sizedBench n gen $ nf (buildSufT (const (1 :: Int)) const (+)) where
    gen = evalR $ C.pack <$> randLowerCaseString n
