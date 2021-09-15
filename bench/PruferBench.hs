module PruferBench where

import Criterion

import Prufer ( graphToSeq, seqToGraph )
import Util ( evalR, randPruferSeq, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Prufer"
    [ -- Convert a Prufer sequence to a tree of n nodes
      bgroup "seqToGraph" $ map benchSeqToGraph sizes
      
      -- Convert a tree of n nodes to a Prufer sequence
    , bgroup "graphToSeq" $ map benchGraphToSeq sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchSeqToGraph :: Int -> Benchmark
benchSeqToGraph n = sizedBench n gen $ nf go where
    gen = evalR $ randPruferSeq n
    go us = seqToGraph (1, n) us

benchGraphToSeq :: Int -> Benchmark
benchGraphToSeq n = sizedBench n gen $ nf graphToSeq where
    gen = evalR $ seqToGraph (1, n) <$> randPruferSeq n
