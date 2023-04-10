{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses #-}
{-|
Arrays for 2-tuples

Primarily useful for unboxed arrays.
If a and b can be put in UArrays, Arr2 UArray UArray i (a,b) works as an unboxed array for (a,b).
This is much better than a boxed Array i (a,b).
Can be nested to store n-tuples. Use with Unbox to store your own defined types.

Implementation notes:
* unsafeAccum and unsafeAccumArray are optional but explicity error for Arr2 because the default
  definitions use STArray. This might be unexpectedly slow with unboxed arrays. Better error than
  TLE. Remove the erroring definition if you don't care.
-}

module Array2
    ( Arr2
    ) where

import Data.Array.Base
import Data.Ix

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
