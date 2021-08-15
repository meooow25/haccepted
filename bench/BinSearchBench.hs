module BinSearchBench where

import Data.Array
import Data.List

import Criterion

import BinSearch
import Util

benchmark :: Benchmark
benchmark = bgroup "BinSearch" [
        -- n searches on a range of size n
        bgroup "binSearch" [
            env (genSearches 100) $ \qs ->
                bench "100" $ whnf (doSearches qs) 100
          , env (genSearches 10000) $ \qs ->
                bench "10000" $ whnf (doSearches qs) 10000
          , env (genSearches 1000000) $ \qs ->
                bench "1000000" $ whnf (doSearches qs) 1000000
        ]

        -- n searches on an array of size n
      , bgroup "binSearchA" [
            env (genArrayAndSearches 100) $ \ ~(qs, a) ->
                bench "100" $ whnf (doSearchesA qs) a
          , env (genArrayAndSearches 10000) $ \ ~(qs, a) ->
                bench "10000" $ whnf (doSearchesA qs) a
          , env (genArrayAndSearches 1000000) $ \ ~(qs, a) ->
                bench "1000000" $ whnf (doSearchesA qs) a
        ]
    ]

genSearches :: Int -> IO [Int]
genSearches n = return $ randIntsR (1, n) n

genArrayAndSearches :: Int -> IO ([Int], Array Int Int)
genArrayAndSearches n = return (randIntsR (1, n) n, listArray (1, n) [1..n])

doSearches :: [Int] -> Int -> ()
doSearches qs n = foldl' (\_ i -> binSearch (>= i) 1 n `seq` ()) () qs

doSearchesA :: [Int] -> Array Int Int -> ()
doSearchesA qs a = foldl' (\_ i -> binSearchA (>= i) a `seq` ()) () qs
