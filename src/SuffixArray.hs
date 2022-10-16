{-
Suffix array
-}

module SuffixArray
    ( buildSufArray
    , buildSufArrayL
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Function
import Data.List
import Data.Ord

import Sort ( sortUBy, countingSort )

type Chr = Int
type Index = Int

buildSufArray :: Chr -> Int -> (Int -> Chr) -> UArray Int Index
buildSufArray b = buildSufArray_ (countingSort (b + 1))

buildSufArrayL :: Int -> (Int -> Chr) -> UArray Int Index
buildSufArrayL n = buildSufArray_ (\f -> listArray (0, n-1) . sortUBy (comparing f) . elems) n

buildSufArray_ :: ((Int -> Chr) -> UArray Int Index -> UArray Int Index)
               -> Int -> (Int -> Chr) -> UArray Int Index
buildSufArray_ sortf n at = sufArrayDoubling n p0 r0 where
    p0 = sortf at' (listArray (0, n-1) [0..])
    r0 = rank p0 ((==) `on` at')
    lastc = at (n - 1)
    at' i | i == n - 1 = lastc
    at' i = let c = at i in c + fromEnum (c >= lastc)

sufArrayDoubling :: Int -> UArray Int Index -> UArray Index Int -> UArray Int Index
sufArrayDoubling n p0 r0 = sa where
    (sa, _) = foldl' (uncurry step) (p0, r0) $ takeWhile (<n) $ iterate (*2) 1
    step :: UArray Int Index -> UArray Index Int -> Int -> (UArray Int Index, UArray Index Int)
    step p r m = r `seq` (p', r') where
        p' = runSTUArray $ do
            pos <- newListArray (0, n-1) [0..] :: ST s (STUArray s Int Int)
            pa <- thaw p
            forM_ [i - m | i <- elems p, i >= m] $ \i -> do
                x <- readArray pos (r!i)
                writeArray pos (r!i) (x + 1)
                writeArray pa x i
            pure pa
        r' = rank p' $ \i j -> r!i == r!j && i + m < n && r!(i + m) == r!(j + m)

rank :: UArray Int Index -> (Index -> Index -> Bool) -> UArray Index Int
rank p eq = array (0, n') $ (p!0, 0) : foldr f (const []) [1..n'] 0 where
    (0, n') = bounds p
    f i k prv = let cur = if eq (p!(i - 1)) (p!i) then prv else i
                in cur `seq` (p!i, cur) : k cur
{-# INLINE rank #-}

--------------------------------------------------------------------------------
-- For tests

{-# INLINABLE buildSufArray #-}
{-# INLINABLE buildSufArrayL #-}
