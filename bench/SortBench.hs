module SortBench where

import qualified Data.List as L

import Criterion

import Sort ( sort, sortU )
import Util ( evalR, randInts, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Sort"
    [ -- Sort n ints
      bgroup "sort" $ map (benchSort sort) sizes
    
      -- Sort n ints
    , bgroup "sortU" $ map (benchSort sortU) sizes

      -- Data.List.sort n ints
    , bgroup "Data.List.sort" $ map (benchSort L.sort) sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchSort :: ([Int] -> [Int]) -> Int -> Benchmark
benchSort sortF n = sizedBench n gen $ nf sortF where
    gen = evalR $ randInts n
