{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables,  TypeApplications, TypeFamilies #-}
module ArrayBench where

import Control.DeepSeq
import Data.Array.IArray
import Data.Foldable

import Criterion

import Array ( UArr, UArr2, Unbox(..) )
import ArrayNFData ()
import Util ( evalR, randInts, randIntsR, shuffle, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Array"
    [ bgroup "Arr"
        [ -- Create an array of size n from a list
          bgroup "listArray @UArr @X" $ map (benchListArray @UArr X) sizes

          -- Uncomment to compare against alternatives
        -- , bgroup "listArray @UArray @Int" $ map (benchListArray @UArray id) sizes
        -- , bgroup "listArray @Array @X" $ map (benchListArray @Array X) sizes

          -- Create an array of size n from an assoc list
        , bgroup "array @UArr @X" $ map (benchArray @UArr X) sizes

        -- , bgroup "array @UArray @Int" $ map (benchArray @UArray id) sizes
        -- , bgroup "array @Array @X" $ map (benchArray @Array X) sizes

          -- Create an array of size n from an assoc list via accumArray
        , bgroup "accumArray @UArr @X" $ map (benchAccumArray @UArr X) sizes

        -- , bgroup "accumArray @UArray @Int" $ map (benchAccumArray @UArray id) sizes
        -- , bgroup "accumArray @Array @X" $ map (benchAccumArray @Array id) sizes

          -- Read n indices in an array of size n
        , bgroup "(!) @UArr @X" $ map (benchAt @UArr X) sizes

        -- , bgroup "(!) @UArray @Int" $ map (benchAt @UArray id) sizes
        -- , bgroup "(!) @Array @X" $ map (benchAt @Array X) sizes
        ]
    , bgroup "Arr2"
        [ -- Create an array of size n from a list
          bgroup "listArray @UArr2 @(Int, Int)" $ map (benchListArray @UArr2 rep) sizes

        -- , bgroup "listArray @Array @(Int, Int)" $ map (benchListArray @Array rep) sizes

          -- Create an array of size n from an assoc list
        , bgroup "array @UArr2 @(Int, Int)" $ map (benchArray @UArr2 rep) sizes

        -- , bgroup "array @Array @(Int, Int)" $ map (benchArray @Array rep) sizes

          -- Read n indices in an array of size n
        , bgroup "(!) @UArr2 @(Int, Int)" $ map (benchAt @UArr2 rep) sizes

        -- , bgroup "(!) @Array @(Int, Int)" $ map (benchAt @Array rep) sizes
        ]
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

newtype X = X Int

instance NFData X where
    rnf = rwhnf

instance Unbox X where
    type Unboxed X = Int

rep :: a -> (a,a)
rep x = (x,x)

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
