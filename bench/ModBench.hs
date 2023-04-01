{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
module ModBench where

import Control.DeepSeq
import Data.List

import Criterion

import Mod ( M7 )
import Util ( evalR, randInts, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Mod"
    [ -- Add n random ints
      bgroup "add" $ map (benchAdd @M7) sizes

      -- Subtract n random ints from 1
    , bgroup "sub" $ map (benchSub @M7) sizes

      -- Multiply n random ints
    , bgroup "mul" $ map (benchMul @M7) sizes

      -- Divide 1 by n random ints
    , bgroup "div" $ map (benchDiv @M7) sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

-- foldl' used instead of sum and product because sum and product are strict
-- staring GHC 9. Most online judges are at 8.10.x.

benchAdd :: forall a. (Num a, NFData a) => Int -> Benchmark
benchAdd n = sizedBench n gen $ nf (foldl' (+) 0) where
    gen = map fromIntegral $ evalR $ randInts n :: [a]

benchSub :: forall a. (Num a, NFData a) => Int -> Benchmark
benchSub n = sizedBench n gen $ nf (foldl' (-) 0) where
    gen = map fromIntegral $ evalR $ randInts n :: [a]

benchMul :: forall a. (Num a, NFData a) => Int -> Benchmark
benchMul n = sizedBench n gen $ nf (foldl' (*) 1) where
    gen = map fromIntegral $ evalR $ randInts n :: [a]

benchDiv :: forall a. (Fractional a, NFData a) => Int -> Benchmark
benchDiv n = sizedBench n gen $ whnf (foldl' (/) 1) where
    gen = map fromIntegral $ evalR $ randInts n :: [a]
    -- lucky no 0
