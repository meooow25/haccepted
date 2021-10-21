{-
Knuth-Morris-Pratt algorithm

A string matching algorithm that generates a failure function p from an input string s, where p(i)
is the length of the longest prefix of s that ends at index i, excluding the prefix [0..i].

Sources:
* https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
* https://cp-algorithms.com/string/prefix-function.html

failFunc
Constructs the failure function. The input sequence should be 0-indexed. O(n).

failFuncBS
failFunc for a ByteString. O(n).
-}

module KMP
    ( failFunc
    , failFuncBS
    ) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C

failFunc :: Eq a => Int -> (Int -> a) -> UArray Int Int
failFunc n at = runSTUArray $ do
    p <- newArray (0, n - 1) 0
    forM_ [1 .. n-1] $ \i -> do
        let f j | j == 0 || at i == at j = pure j
                | otherwise              = f =<< readArray p (j - 1)
        j <- f =<< readArray p (i - 1)
        writeArray p i $ j + fromEnum (at i == at j)
    pure p

failFuncBS :: C.ByteString -> UArray Int Int
failFuncBS bs = failFunc (C.length bs) (C.index bs)
