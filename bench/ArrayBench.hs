{-# LANGUAGE TypeApplications, TypeFamilies #-}
module ArrayBench where

import Control.DeepSeq

import Criterion

import Array ( UArr, Unbox(..) )
import ArrayBenches ( benchAccumArray, benchArray, benchAt, benchListArray ) 
import ArrayNFData ()

benchmark :: Benchmark
benchmark = bgroup "Array"
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

sizes :: [Int]
sizes = [100, 10000, 1000000]

newtype X = X Int

instance NFData X where
    rnf = rwhnf

instance Unbox X where
    type Unboxed X = Int
