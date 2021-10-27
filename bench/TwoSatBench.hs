module TwoSatBench where

import Control.Monad.Random

import Criterion

import TwoSat ( Var(..), solve2Sat )
import Util ( evalR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "TwoSat"
    [ -- Solve 2-sat for n variables and n clauses
      bgroup "solve2Sat" $ map benchSolve2Sat sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchSolve2Sat :: Int -> Benchmark
benchSolve2Sat n = sizedBench n gen $ nf $ solve2Sat (1, n) where
    genVar = do
        b <- getRandom
        x <- getRandomR (1, n)
        pure $ if b then Id x else Not x
    gen = evalR $ replicateM n $ (,) <$> genVar <*> genVar
