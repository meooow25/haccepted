{-# LANGUAGE TypeApplications #-}
module MathBench where

import Control.DeepSeq
import Data.List

import Criterion

import Array ( UArr )
import Math ( egcd, egcd2, mkFactorials, mkInvFactorials )
import MInt ( MInt )
import Util ( evalR, randInts, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "Math"
    [ -- Run egcd on n random pairs of Ints
      bgroup "egcd" $ map (benchEgcdf egcd) sizes

      -- Run egcd2 on n random pairs of Ints
    , bgroup "egcd2" $ map (benchEgcdf egcd2) sizes

      -- Calculate factorials for 0..n
    , bgroup "mkFactorials" $ map benchMkFactorials sizes

      -- Calculate inverse factorials for 0..n
    , bgroup "mkInvFactorials" $ map benchMkInvFactorials sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 1000000]

benchEgcdf :: NFData a => (Int -> Int -> a) -> Int -> Benchmark
benchEgcdf f n = sizedBench n gen $ nf go where
    gen = evalR $ zip <$> randInts n <*> randInts n
    go = foldl' (\_ xy -> uncurry f xy `deepseq` ()) ()

benchMkFactorials :: Int -> Benchmark
benchMkFactorials n = sizedBench n n $ whnf (mkFactorials @UArr @MInt)

benchMkInvFactorials :: Int -> Benchmark
benchMkInvFactorials n = sizedBench n gen $ whnf (mkInvFactorials @UArr @MInt n) where
    gen = product (map fromIntegral [1..n]) :: MInt
