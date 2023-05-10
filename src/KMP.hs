{-|
Knuth-Morris-Pratt algorithm

A string matching algorithm that generates a prefix function p from an input string s, where p(i)
is the length of the longest prefix of s that ends at index i, excluding the prefix [0..i].

Sources:
* https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
* https://cp-algorithms.com/string/prefix-function.html

prefixFunc
Constructs the prefix function. The input sequence should be 0-indexed. O(n).

prefixFuncBS
prefixFunc for a ByteString. O(n).
-}

module KMP
    ( prefixFunc
    , prefixFuncBS
    ) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C

prefixFunc :: Eq a => Int -> (Int -> a) -> UArray Int Int
prefixFunc n at = runSTUArray $ do
    p <- newArray (0, n-1) 0
    forM_ [1 .. n-1] $ \i -> do
        let f j | at i == at j = pure (j+1)
            f 0 = pure 0
            f j = readArray p (j-1) >>= f
        readArray p (i-1) >>= f >>= writeArray p i
    pure p

prefixFuncBS :: C.ByteString -> UArray Int Int
prefixFuncBS bs = prefixFunc (C.length bs) (C.index bs)
