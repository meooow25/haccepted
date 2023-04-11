{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
module ArrayBenches where

import Control.DeepSeq
import Data.Array.IArray
import Data.List

import Criterion

import Util ( evalR, randInts, randIntsR, shuffle, sizedBench )

benchListArray :: forall arr a. (NFData a, NFData (arr Int a), IArray arr a)
               => (Int -> a) -> Int -> Benchmark
benchListArray fromInt n = sizedBench n gen $ nf (listArray @arr @a (1,n)) where
    gen = evalR $ map fromInt <$> randInts n
{-# INLINABLE benchListArray #-}

benchArray :: forall arr a. (NFData a, NFData (arr Int a), IArray arr a)
           => (Int -> a) -> Int -> Benchmark
benchArray fromInt n = sizedBench n gen $ nf (array @arr @a (1,n)) where
    gen = evalR $ zip <$> shuffle [1..n] <*> (map fromInt <$> randInts n)
{-# INLINABLE benchArray #-}

benchAccumArray :: forall arr a. (NFData a, NFData (arr Int a), IArray arr a)
                => (Int -> a) -> Int -> Benchmark
benchAccumArray fromInt n =
    sizedBench n gen $ nf (accumArray @arr @a (const id) (fromInt 0) (1,n))
  where
    gen = evalR $ zip <$> randIntsR (1,n) n <*> (map fromInt <$> randInts n)
{-# INLINABLE benchAccumArray #-}

benchAt :: forall arr a. (NFData a, NFData (arr Int a), IArray arr a)
        => (Int -> a) -> Int -> Benchmark
benchAt fromInt n = sizedBench n gen $ whnf go where
    gen = evalR $ (,) <$> (listArray @arr (1,n) . map fromInt <$> randInts n) <*> randIntsR (1,n) n
    go (a, is) = foldl' (flip $ deepseq . (a!)) () is
{-# INLINABLE benchAt #-}
