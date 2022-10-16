{-
Suffix array
-}

module SuffixArray
    ( buildSufA
    , buildSufAL
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
type SuffixId = Int

buildSufA :: Chr -> Int -> (Int -> Chr) -> (UArray Int SuffixId, UArray Int Int)
buildSufA b = buildSufA_ (countingSort (b + 1))

buildSufAL :: Int -> (Int -> Chr) -> (UArray Int SuffixId, UArray Int Int)
buildSufAL n = buildSufA_ (\f -> listArray (0, n-1) . sortUBy (comparing f) . elems) n

buildSufA_ :: ((Int -> Chr) -> UArray Int SuffixId -> UArray Int SuffixId)
           -> Int -> (Int -> Chr) -> (UArray Int SuffixId, UArray Int Int)
buildSufA_ sortf n at = (p, lcp) where
    (p, r) = sufADoubling n p0 r0
    lcp = kasai at p r
    p0 = sortf at' (listArray (0, n-1) [0..])
    r0 = rankSufA p0 ((==) `on` at')
    lastc = at (n - 1)
    at' i | i == n - 1 = lastc
    at' i = let c = at i in c + fromEnum (c >= lastc)

sufADoubling :: Int -> UArray Int SuffixId -> UArray SuffixId Int -> (UArray Int SuffixId, UArray Int Int)
sufADoubling n p0 r0 = foldl' (uncurry step) (p0, r0) $ takeWhile (<n) $ iterate (*2) 1 where
    step :: UArray Int SuffixId -> UArray SuffixId Int -> Int -> (UArray Int SuffixId, UArray SuffixId Int)
    step p r m = r `seq` (p', r') where
        p' = runSTUArray $ do
            pos <- newListArray (0, n-1) [0..] :: ST s (STUArray s Int Int)
            pa <- thaw p
            forM_ [i - m | i <- elems p, i >= m] $ \i -> do
                x <- readArray pos (r!i)
                writeArray pos (r!i) (x + 1)
                writeArray pa x i
            pure pa
        r' = rankSufA p' $ \i j -> r!i == r!j && i + m < n && r!(i + m) == r!(j + m)

rankSufA :: UArray Int SuffixId -> (SuffixId -> SuffixId -> Bool) -> UArray SuffixId Int
rankSufA p eq = array (0, n') $ (p!0, 0) : foldr f (const []) [1..n'] 0 where
    (0, n') = bounds p
    f i k prv = let cur = if eq (p!(i - 1)) (p!i) then prv else i
                in cur `seq` (p!i, cur) : k cur
{-# INLINE rankSufA #-}

kasai :: (Int -> Chr) -> UArray Int SuffixId -> UArray SuffixId Int -> UArray Int Int
kasai at p r = array (0, n'-1) $ foldr f (const []) [0..n'] 0 where
    (0, n') = bounds p
    f i k _ | r!i == n' = k 0
    f i k x = (r!i, x') : k (max 0 (x' - 1)) where
        j = p!(r!i + 1)
        x' = until (\x'' -> i + x'' > n' || j + x'' > n' || at (i + x'') /= at (j + x'')) (+1) x

--------------------------------------------------------------------------------
-- For tests

{-# INLINABLE buildSufA #-}
{-# INLINABLE buildSufAL #-}
