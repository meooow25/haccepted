module FenwickBench where

import Data.List
import Data.Monoid

import Criterion

import Fenwick
import Util

benchmark :: Benchmark
benchmark = bgroup "Fenwick" [
      -- Build a Fenwick tree of size n
        bgroup "buildF" [
            bench "100" $ nf genF 100
          , bench "10000" $ nf genF 10000
          , bench "1000000" $ nf genF 1000000
        ]
      
      -- n updates on a Fenwick tree of size n
      , bgroup "updateF" [
            env (return (genF 100, genUpds 100)) $ \ ~(ft, us) ->
                bench "100" $ nf (applyUpds us) ft
          , env (return (genF 10000, genUpds 10000)) $ \ ~(ft, us) ->
                bench "10000" $ nf (applyUpds us) ft
          , env (return (genF 1000000, genUpds 1000000)) $ \ ~(ft, us) ->
                bench "1000000" $ nf (applyUpds us) ft
        ]
      
      -- n queries on a Fenwick tree of size n
      , bgroup "queryF" [
            env (return (genF 100, genQrys 100)) $ \ ~(ft, qs) ->
                bench "100" $ nf (applyQrys qs) ft
          , env (return (genF 10000, genQrys 10000)) $ \ ~(ft, qs) ->
                bench "10000" $ nf (applyQrys qs) ft
          , env (return (genF 1000000, genQrys 1000000)) $ \ ~(ft, qs) ->
                bench "1000000" $ nf (applyQrys qs) ft
        ]
    ]

genF :: Int -> FTree (Sum Int)
genF n = buildF (1, n)

genUpds :: Int -> [(Int, Sum Int)]
genUpds n = zip (randIntsR (1, n) n) (Sum <$> randInts n)

genQrys :: Int -> [Int]
genQrys n = randIntsR (1, n) n

applyUpds :: [(Int, Sum Int)] -> FTree (Sum Int) -> FTree (Sum Int)
applyUpds upds ft = foldl' (\ft (i, x) -> updateF i x ft) ft upds

applyQrys :: [Int] -> FTree (Sum Int) -> ()
applyQrys qs ft = foldl' (\_ i -> queryF i ft `seq` ()) () qs
