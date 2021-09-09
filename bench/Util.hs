module Util where

import Control.DeepSeq
import Control.Monad.Random
import Data.Graph
import Data.List
import Data.Tree

import Criterion
import qualified System.Random.Shuffle as Shuffle

import Prufer ( seqToGraph )

gen :: StdGen
gen = mkStdGen 42

randIntsR :: (Int, Int) -> Int -> [Int]
randIntsR bnds n = take n $ randomRs bnds gen

randInts :: Int -> [Int]
randInts n = take n $ randoms gen

randIntPairsR :: (Int, Int) -> Int -> [(Int, Int)]
randIntPairsR bnds n = uncurry zip $ splitAt n $ randIntsR bnds $ 2 * n

randSortedIntPairsR :: (Int, Int) -> Int -> [(Int, Int)]
randSortedIntPairsR bnds n = minmax <$> randIntPairsR bnds n where
    minmax (x, y) = (min x y, max x y)

randPruferSeq :: Int -> [Int]
randPruferSeq n = drop 2 $ shuffle [1..n]

randTree :: Int -> Tree Vertex
randTree n = t where
    g = seqToGraph (1, n) $ randPruferSeq n
    [t] = dfs g [n]

randForest :: Int -> [Tree Vertex]
randForest n = subForest $ randTree $ n + 1

shuffle :: [a] -> [a]
shuffle xs = Shuffle.shuffle' xs (length xs) gen

type RandStd = Rand StdGen

-- TODO: Replace the functions not using Rand
randInts1 :: Int -> RandStd [Int]
randInts1 n = replicateM n getRandom

shuffle1 :: [a] -> RandStd [a]
shuffle1 xs = map fst . sortOn snd . zip xs <$> randInts1 (length xs)

evalR :: RandStd a -> a
evalR = flip evalRand gen

sizedBench :: NFData env => Int -> env -> (env -> Benchmarkable) -> Benchmark
sizedBench n e b = env (return e) $ bench (show n) . b
{-# INLINE sizedBench #-}
