module ConvexHullBench where

import Criterion

import ConvexHull ( convexHull )
import Geometry ( V2(..) )
import Util ( evalR, randIntsR, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "ConvexHull"
    [ -- Calculate the convex hull for n points
      bgroup "convexHull" $ map benchConvexHull sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

benchConvexHull :: Int -> Benchmark
benchConvexHull n = sizedBench n gen $ nf convexHull where
    gen = evalR $ zipWith V2 <$> randIntsR bnds n <*> randIntsR bnds n
    limit = 10 ^ (9 :: Int)
    bnds = (-limit, limit)
