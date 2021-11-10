{-
Mo's algorithm

Useful for answering certain range queries on a static sequence. The algorithm works by maintaining
some state for a current range, and updating the state by adding or removing elements one by one.
An important parameter for the algorithm is the block size. If the range size is n, number of
queries is q, block size is b, state update takes time f(n), getting the answer for the current
state takes g(n), then the algorithm answers all queries in
    O(q log q + bq * f(n) + n^2/b * f(n) + q * g(n))
A suitable value of block size minimizes run time.
For example, when q ~ n, f(n) = O(1), g(n) = O(1), setting b = sqrt(n) gives run time O(n sqrt(n)).
For f(n) = O(log n), a better choice of b is sqrt(n/log n) giving run time O(n sqrt(n log n)). In
practice, it works to experiment a bit and choose a good fixed value.

Sources:
* https://cp-algorithms.com/data_structures/sqrt_decomposition.html

runMo
Run Mo's algorithm given the block size, state update and answer functions, and queries. See above
for time complexity.

sqrtSize
Square root of an Int, rounded to an Int. O(1).
-}

module Mo
    ( MoQuery(..)
    , Tag
    , runMo
    , sqrtSize
    ) where

import Control.DeepSeq
import Control.Monad
import Data.List

type Tag = Int
data MoQuery = MoQuery { ql_ :: !Int, qr_ :: !Int, qtag_ :: !Tag } deriving Show

runMo :: Monad m => Int -> (Int -> m ()) -> (Int -> m ()) -> m a -> [MoQuery] -> m [(Tag, a)]
runMo _     _   _   _   []   = pure []
runMo bsize add rem ans qrys = snd <$> foldM f ((start, start-1), []) qrys' where
    cmp (MoQuery l1 r1 _) (MoQuery l2 r2 _) = compare b1 b2 <> rc where
        (b1, b2) = (l1 `div` bsize, l2 `div` bsize)
        rc = if even b1 then compare r1 r2 else compare r2 r1
    qrys' = sortBy cmp qrys
    MoQuery start _ _ = head qrys'
    f ((l, r), acc) (MoQuery ql qr qtag) = do
        mapM_ add $ [l-1, l-2 .. ql] ++ [r+1 .. qr]
        mapM_ rem $ [l .. ql-1] ++ [r, r-1 .. qr+1]
        x <- ans
        pure ((ql, qr), (qtag, x):acc)

sqrtSize :: Int -> Int
sqrtSize n = round $ sqrt (fromIntegral n :: Double)

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE runMo #-}

instance NFData MoQuery where
    rnf = rwhnf
