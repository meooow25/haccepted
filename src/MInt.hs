{-# LANGUAGE MultiParamTypeClasses #-}
{-|

MInt
Integer type for modular arithmetic, using fixed prime modulo.

Implementation notes:
* MInt is a newtype of Int, change to Int64 if running on 32-bit.
* Regarding unsafeCoerce (trust me it's safe!), type roles for unboxed arrays are nominal (and not
  representational), so trying to derive unboxed arrays for MInt does not work (see
  https://gitlab.haskell.org/ghc/ghc/-/issues/9220). As I see it my options are
    a. spill more guts out of Array.Base and Array.IO to copy-paste everything an Int array does
    b. use unsafeCoerce
  I would love to hear of a better way.

Instances of Eq, Num, Fractional exist for MInt. All the usual operations take O(1) time, except for
recip which takes O(log m) time.
An instance of Enum exists for MInt. The enum is cyclic, it wraps to 0 after m-1.
An instance of IArray UArray MInt exists. Instances of MArray (STUArray s) MInt (ST s) and
MArray IOUArray MInt IO are also defined.

mkMInt
Int to MInt. The MInt constructor can be used directly if the value is known to be in [0.. m-1].
O(1).

inv
Reciprocal of an MInt. O(log m).
-}

module MInt
    ( MInt(..)
    , mkMInt
    , inv
    ) where

import Control.Monad.ST
import Data.Array.Base
import Data.Array.IO
import Data.Coerce
import Data.Ratio
import Unsafe.Coerce

m :: Int
m = 1000000007
-- m = 998244353

newtype MInt = MInt { toInt :: Int } deriving (Eq, Show)

mkMInt :: Int -> MInt
mkMInt = MInt . (`mod` m)

instance Num MInt where
    MInt a + MInt b = MInt $ let c = a + b in if c >= m then c - m else c
    MInt a - MInt b = MInt $ let c = a - b in if c < 0 then c + m else c
    MInt a * MInt b = MInt $ a * b `mod` m
    abs             = id
    signum          = MInt . signum . toInt
    fromInteger     = MInt . fromInteger . (`mod` fromIntegral m)

inv :: MInt -> MInt
inv = (^(m - 2))

instance Fractional MInt where
    recip          = (^(m - 2))
    fromRational r = fromIntegral (numerator r) / fromIntegral (denominator r)

instance Enum MInt where
    toEnum                 = fromIntegral
    fromEnum               = toInt
    enumFromTo x y         = takeWhile (/= y + 1) [x..]
    enumFromThenTo x1 x2 y = takeWhile (/= y + 1) [x1, x2 ..]

--------------------------------------------------------------------------------
-- Unboxed array support

coerceUIM :: UArray i Int -> UArray i MInt
coerceUIM = unsafeCoerce

coerceUMI :: UArray i MInt -> UArray i Int
coerceUMI = unsafeCoerce

instance IArray UArray MInt where
    bounds                = bounds . coerceUMI
    numElements           = numElements . coerceUMI
    unsafeArray lu ies    = coerceUIM $ unsafeArray lu (coerce ies)
    unsafeAt arr i        = coerce $ unsafeAt (coerceUMI arr) i
    unsafeReplace arr ies = coerceUIM $ unsafeReplace (coerceUMI arr) (coerce ies)
    unsafeAccum f arr ies = coerceUIM $ unsafeAccum (coerce f) (coerceUMI arr) ies
    unsafeAccumArray f initialValue lu ies
                          = coerceUIM $ unsafeAccumArray (coerce f) (coerce initialValue) lu ies

coerceSTIM :: STUArray s i Int -> STUArray s i MInt
coerceSTIM = unsafeCoerce

coerceSTMI :: STUArray s i MInt -> STUArray s i Int
coerceSTMI = unsafeCoerce

instance MArray (STUArray s) MInt (ST s) where
    getBounds           = getBounds . coerceSTMI
    getNumElements      = getNumElements . coerceSTMI
    unsafeNewArray_     = fmap coerceSTIM . unsafeNewArray_
    newArray_           = fmap coerceSTIM . newArray_
    unsafeRead arr i    = coerce <$> unsafeRead (coerceSTMI arr) i
    unsafeWrite arr i e = unsafeWrite (coerceSTMI arr) i (coerce e)

coerceIOIM :: IOUArray i Int -> IOUArray i MInt
coerceIOIM = unsafeCoerce

coerceIOMI :: IOUArray i MInt -> IOUArray i Int
coerceIOMI = unsafeCoerce

instance MArray IOUArray MInt IO where
    getBounds                = getBounds . coerceIOMI
    getNumElements           = getNumElements . coerceIOMI
    newArray lu initialValue = coerceIOIM <$> newArray lu (coerce initialValue)
    unsafeNewArray_          = fmap coerceIOIM . unsafeNewArray_
    newArray_                = fmap coerceIOIM . newArray_
    unsafeRead arr i         = coerce <$> unsafeRead (coerceIOMI arr) i
    unsafeWrite arr i e      = unsafeWrite (coerceIOMI arr) i (coerce e)
