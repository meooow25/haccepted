{-|
Suffix array and LCP array

A suffix array is the sorted array of all suffixes of a string, each suffix represented by its
start index. An LCP (longest common prefix) array is an array of the lengths of the longest common
prefixes of consecutive suffixes in the suffix array.
A suffix array, sometimes with its LCP array, can be used to perform tasks like finding patterns
in the string or calculating certain properties of the string.
Other suffix structures, such as suffix trees and suffix automata may serve as alternates to suffix
arrays.

This implementation constructs the suffix array and LCP array from a given string indexing function,
with elements of type Int. It takes O(n log n), which is usually fast enough. O(n) algorithms to
construct suffix arrays exist.

Sources:
* Udi Manber and Gene Myers, "Suffix Arrays: A New Method for On-Line String Searches", 1990
  https://dl.acm.org/doi/10.5555/320176.320218
* Kasai et al., "Linear-Time Longest-Common-Prefix Computation in Suffix Arrays and Its
  Applications", 2001
  https://link.springer.com/chapter/10.1007/3-540-48194-X_17
* https://sites.google.com/site/indy256/algo/suffix_array

Implementation notes:
* The construction is implemented using a prefix doubling algorithm, similar to the algorithm due to
  Manber and Myers. Each step sorts substrings of lengths in successive powers of two.
* The last character is considered smaller than other equal characters. This is one way, and an easy
  one, to allow shorter substrings to appear first in the suffix array.
* At the step sorting substrings of length 2 * m, we go over the substrings in the sorted order of
  their right halves and put them into positons corresponding to the ranks of their left halves.
* The LCP array is constructed from the suffix array using Kasai's algorithm.

buildSufA
Builds a suffix array and LCP array. n is the length of the string. at is a 0-based indexing
function into the string. Characters must be in [0..b-1]. Faster than buildSufAL unless b is too
large. O(b + n log n).

buildSufAL
Builds a suffix array and LCP array. n is the length of the string. at is a 0-based indexing
function into the string. Intended for large alphabets. O(n log n).
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

import Sort ( sortUABy, countingSortUA )

type Chr = Int
type SuffixId = Int

buildSufA :: Chr -> Int -> (Int -> Chr) -> (UArray Int SuffixId, UArray Int Int)
buildSufA b = buildSufA_ (countingSortUA (b + 1))

buildSufAL :: Int -> (Int -> Chr) -> (UArray Int SuffixId, UArray Int Int)
buildSufAL = buildSufA_ (sortUABy . comparing)

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
