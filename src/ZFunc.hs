{-
Z-function

A string matching algorithm. The Z-algorithm generates the function z from a string s, where z(i)
is the length of the longest prefix of s that starts at i. z[0] is set to 0.

Sources:
* https://cp-algorithms.com/string/z-function.html

zFunc
Constructs the Z-function. The input sequence should be 0-indexed. O(n).

zFuncBS
zFunc for a ByteString. O(n).
-}

module ZFunc
    ( zFunc
    , zFuncBS
    ) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C

zFunc :: Eq a => Int -> (Int -> a) -> UArray Int Int
zFunc n at = runSTUArray $ do
    z <- newArray (0, n - 1) 0
    let go lr@(l, r) i = do
            j <- max 0 . min (r - i) <$> readArray z (i - l)
            let k = head $ dropWhile (\k -> k < n && at (k - i) == at k) [i+j..]
            writeArray z i $ k - i
            pure $ if k > r then (i, k) else lr
    foldM_ go (0, 0) [1 .. n-1]
    pure z

zFuncBS :: C.ByteString -> UArray Int Int
zFuncBS bs = zFunc (C.length bs) (C.index bs)
