module Util where

import System.Random

seed :: Int
seed = 42

randIntsR :: (Int, Int) -> Int -> [Int]
randIntsR bnds n = take n $ randomRs bnds $ mkStdGen seed

randInts :: Int -> [Int]
randInts n = take n $ randoms $ mkStdGen seed
