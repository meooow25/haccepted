{-# LANGUAGE TypeApplications #-}
module MIntBench where

import Criterion

import MInt ( MInt )

import ModBench ( benchAdd, benchSub, benchMul, benchDiv )

benchmark :: Benchmark
benchmark = bgroup "MInt"
    [ -- Add n random ints
      bgroup "add" $ map (benchAdd @MInt) sizes

      -- Subtract n random ints from 1
    , bgroup "sub" $ map (benchSub @MInt) sizes

      -- Multiply n random ints
    , bgroup "mul" $ map (benchMul @MInt) sizes

      -- Divide 1 by n random ints
    , bgroup "div" $ map (benchDiv @MInt) sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]
