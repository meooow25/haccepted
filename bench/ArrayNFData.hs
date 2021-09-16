module ArrayNFData where

import Control.DeepSeq
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed

-- Unboxed arrays are strict

instance NFData (UArray i e) where
    rnf = rwhnf

instance NFData (STUArray s i e) where
    rnf = rwhnf

instance NFData (IOUArray i e) where
    rnf = rwhnf
