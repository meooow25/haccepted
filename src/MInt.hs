{-# LANGUAGE TypeFamilies #-}
{-|
Modular arithmetic

MInt is a newtype of Int for arithmetic modulo a known fixed prime.
For a more general type see Mod.hs.

Implementation notes:
* MInt is a newtype of Int, change to Int64 if running on 32-bit.

Instances of Eq, Num, Fractional exist for MInt. All the usual operations take O(1) time, except for
recip which takes O(log m) time.
An instance of Enum exists for MInt. The enum is cyclic, it wraps to 0 after m-1.
Unboxed array support is available via Unbox.

MInt
Int type for arithmetic modulo a fixed prime mm.

mm
The prime modulus.
-}

module MInt
    ( MInt(..)
    , mm
    ) where

import Control.DeepSeq
import Data.Ratio

import Array ( Unbox(..) )

mm :: Int
mm = 1000000007
-- m = 998244353

newtype MInt = MInt { unMInt :: Int } deriving (Eq, Ord, Show)

instance Num MInt where
    MInt a + MInt b = MInt $ let c = a + b in if c >= mm then c - mm else c
    MInt a - MInt b = MInt $ let c = a - b in if c < 0 then c + mm else c
    MInt a * MInt b = MInt $ a * b `mod` mm
    abs             = id
    signum          = MInt . signum . unMInt
    fromInteger     = MInt . fromInteger . (`mod` fromIntegral mm)

instance Fractional MInt where
    recip          = (^(mm - 2))
    fromRational r = fromIntegral (numerator r) / fromIntegral (denominator r)

instance Enum MInt where
    toEnum                 = fromIntegral
    fromEnum               = unMInt
    enumFromTo x y         = takeWhile (/= y + 1) [x..]
    enumFromThenTo x1 x2 y = takeWhile (/= y + 1) [x1, x2 ..]

instance Unbox MInt where
    type Unboxed MInt = Int

--------------------------------------------------------------------------------
-- For tests

instance NFData MInt where
    rnf = rwhnf
