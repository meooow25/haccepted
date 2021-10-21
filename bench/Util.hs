module Util where

import Control.DeepSeq
import Control.Monad.Random
import Data.Char
import Data.Graph
import Data.List
import Data.Tree

import Criterion

import Prufer ( seqToGraph )

type RandStd = Rand StdGen

gen :: StdGen
gen = mkStdGen 42

randInts :: Int -> RandStd [Int]
randInts n = replicateM n getRandom

randIntsR :: (Int, Int) -> Int -> RandStd [Int]
randIntsR bnds n = replicateM n $ getRandomR bnds

randIntPairsR :: (Int, Int) -> Int -> RandStd [(Int, Int)]
randIntPairsR bnds n = zip <$> randIntsR bnds n <*> randIntsR bnds n

randSortedIntPairsR :: (Int, Int) -> Int -> RandStd [(Int, Int)]
randSortedIntPairsR bnds n = map minmax <$> randIntPairsR bnds n where
    minmax (x, y) = (min x y, max x y)

shuffle :: [a] -> RandStd [a]
shuffle xs = map fst . sortOn snd . zip xs <$> randInts (length xs)

randPruferSeq :: Int -> RandStd [Int]
randPruferSeq n = drop 2 <$> shuffle [1..n]

randTree :: Int -> RandStd (Tree Vertex)
randTree n = do
    g <- seqToGraph (1, n) <$> randPruferSeq n
    let [t] = dfs g [n]
    pure t

randForest :: Int -> RandStd [Tree Vertex]
randForest n = subForest <$> randTree (n + 1)

randASCIIString :: Int -> RandStd String
randASCIIString n = replicateM n $ chr <$> getRandomR (0, 127)

evalR :: RandStd a -> a
evalR = flip evalRand gen

sizedBench :: NFData env => Int -> env -> (env -> Benchmarkable) -> Benchmark
sizedBench n e b = env (return e) $ bench (show n) . b
{-# INLINE sizedBench #-}
