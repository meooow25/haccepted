{-# LANGUAGE TypeApplications #-}
module Array2Bench where

import Data.Array.Unboxed

import Criterion

import Array2 ( Arr2 )
import ArrayBenches ( benchArray, benchAt, benchListArray ) 
import ArrayNFData ()

benchmark :: Benchmark
benchmark = bgroup "Array2"
    [ -- Create an array of size n from a list
      bgroup "listArray @UArr2 @(Int, Int)" $ map (benchListArray @UArr2 rep) sizes

      -- Uncomment to compare against alternatives
    -- , bgroup "listArray @Array @(Int, Int)" $ map (benchListArray @Array rep) sizes

      -- Create an array of size n from an assoc list
    , bgroup "array @UArr2 @(Int, Int)" $ map (benchArray @UArr2 rep) sizes

    -- , bgroup "array @Array @(Int, Int)" $ map (benchArray @Array rep) sizes

      -- Read n indices in an array of size n
    , bgroup "(!) @UArr2 @(Int, Int)" $ map (benchAt @UArr2 rep) sizes

    -- , bgroup "(!) @Array @(Int, Int)" $ map (benchAt @Array rep) sizes
    ]

type UArr2 = Arr2 UArray UArray

rep :: a -> (a,a)
rep x = (x,x)

sizes :: [Int]
sizes = [100, 10000, 1000000]
