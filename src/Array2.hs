{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses #-}
{-|
Arrays for 2-tuples

Primarily intended as a way to get unboxed arrays.
If a and b can be put in UArrays, UArr2 i (a,b) works as an unboxed array for (a,b).
This is a lot more performant than a boxed Array i (a,b). Also works with mutable arrays, STUArr2
and IOUArr2.
Can be nested to get UArr3, UArr4, etc. Use with Unbox and Arr to store your own types.

Implementation notes:
* unsafeAccum and unsafeAccumArray are optional but explicity error for Arr2 because the default
  definitions use STArray. This is horribly slow with unboxed arrays. Better error than TLE.
* Indexing is as fast as the underlying representation but construction via listArray and array are
  known to be slower. See Array2Bench.hs. TODO: Figure out why and fix it.
* TODO: Implement freeze and unsafeFreeze.
-}

module Array2
    ( Arr2
    , UArr2
    , UArr3
    , UArr4
    , STUArr2
    , STUArr3
    , STUArr4
    , IOUArr2
    , IOUArr3
    , IOUArr4
    ) where

import Control.DeepSeq
import Data.Array.Base
import Data.Array.IO

data Arr2 arra arrb i ab where
    Arr2 :: !(arra i a) -> !(arrb i b) -> Arr2 arra arrb i (a, b)

instance (IArray arra a, IArray arrb b) => IArray (Arr2 arra arrb) (a, b) where
    bounds (Arr2 xa _)              = bounds xa
    numElements (Arr2 xa _)         = numElements xa
    unsafeArray b ixys              = Arr2 (unsafeArray b (map (fmap fst) ixys)) (unsafeArray b (map (fmap snd) ixys))
    unsafeAt (Arr2 xa ya) i         = (unsafeAt xa i, unsafeAt ya i)
    unsafeReplace (Arr2 xa ya) ixys = Arr2 (unsafeReplace xa (map (fmap fst) ixys)) (unsafeReplace ya (map (fmap snd) ixys))
    unsafeAccum                     = error "Arr2 unsafeAccum: not implemented"
    unsafeAccumArray                = error "Arr2 unsafeAccumArray: not implemented"

instance (IArray (Arr2 arra arrb) ab, Ix i, Show i, Show ab) => Show (Arr2 arra arrb i ab) where
    showsPrec = showsIArray

instance (Monad m, MArray marra a m, MArray marrb b m) => MArray (Arr2 marra marrb) (a, b) m where
    getBounds (Arr2 xa _)             = getBounds xa
    getNumElements (Arr2 xa _)        = getNumElements xa
    unsafeNewArray_ b                 = Arr2 <$> unsafeNewArray_ b <*> unsafeNewArray_ b
    newArray_ b                       = Arr2 <$> newArray_ b <*> newArray_ b
    unsafeRead (Arr2 xa ya) i         = (,) <$> unsafeRead xa i <*> unsafeRead ya i
    unsafeWrite (Arr2 xa ya) i (x, y) = unsafeWrite xa i x *> unsafeWrite ya i y

type UArr2 = Arr2 UArray UArray
type UArr3 = Arr2 UArray UArr2
type UArr4 = Arr2 UArr2 UArr2

type STUArr2 s = Arr2 (STUArray s) (STUArray s)
type STUArr3 s = Arr2 (STUArray s) (STUArr2 s)
type STUArr4 s = Arr2 (STUArr2 s) (STUArr2 s)

type IOUArr2 = Arr2 IOUArray IOUArray
type IOUArr3 = Arr2 IOUArray IOUArr2
type IOUArr4 = Arr2 IOUArr2 IOUArr2

--------------------------------------------------------------------------------
-- For tests

instance (NFData (arra i a), NFData (arrb i b)) => NFData (Arr2 arra arrb i (a, b)) where
    rnf (Arr2 xa ya) = rnf xa `seq` rnf ya
