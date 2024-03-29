module Util where

import Control.DeepSeq
import Control.Monad.Random
import Data.Array
import Data.Graph
import Data.List
import Data.Tree
import qualified Data.Set as S

import Criterion

import LabelledGraph ( LTree, buildLG, dfsLTree )
import Prufer ( seqToEdges, seqToGraph )

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

randLTree :: Int -> RandStd (LTree Int Vertex)
randLTree n = do
    g <- seqToGraph (1, n) <$> randPruferSeq n
    let es = edges g
    ls <- randInts (length es)
    let g' = buildLG (bounds g) [(u, (l, v)) | ((u, v), l) <- zip es ls]
        t = dfsLTree g' n
    pure t

randConnectedGraph :: Int -> Int -> RandStd Graph
randConnectedGraph n m | m < n - 1 = error "too few edges"
randConnectedGraph n m = do
    treeEdges <- S.fromList . seqToEdges (1, n) <$> randPruferSeq n
    let go edges | S.size edges == 2 * m = pure edges
        go edges = do
            u <- getRandomR (1, n)
            v <- getRandomR (1, n)
            if u == v || S.member (u, v) edges
                then go edges
                else go $ S.insert (u, v) $ S.insert (v, u) edges
    buildG (1, n) . S.elems <$> go treeEdges

randASCIIString :: Int -> RandStd String
randASCIIString n = replicateM n $ getRandomR ('\0', '\127')

randLowerCaseString :: Int -> RandStd String
randLowerCaseString n = replicateM n $ getRandomR ('a', 'z')

evalR :: RandStd a -> a
evalR = flip evalRand gen

sizedBench :: NFData env => Int -> env -> (env -> Benchmarkable) -> Benchmark
sizedBench n e = sizedBenchIO n (pure e)
{-# INLINE sizedBench #-}

sizedBenchIO :: NFData env => Int -> IO env -> (env -> Benchmarkable) -> Benchmark
sizedBenchIO n e b = env e $ bench (show n) . b
{-# INLINE sizedBenchIO #-}
