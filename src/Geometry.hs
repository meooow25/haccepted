{-|
Simple utilities for 2D geometry.

mag2
The square of the magnitude of the vector.

dist2
The square of the distance between two vectors.

dot
Dot product of two vectors.

cross
Cross product of two vectors.

turn
The turn going from p1 to p2 to p3. Returns an Ordering representing left, no turn, or right.
Mnemonic: LT stands for "left turn".
-}

module Geometry
    ( V2(..)
    , mag2
    , dist2
    , dot
    , cross
    , turn
    ) where

import Control.DeepSeq

data V2 = V2 !Int !Int deriving (Eq, Ord, Show)

instance Num V2 where
    V2 x1 y1 + V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
    V2 x1 y1 - V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
    (*)         = error "(*) @V2: unsupported"
    abs         = error "abs @V2: unsupported"
    signum      = error "signum @V2: unsupported"
    fromInteger = error "fromInteger @V2: unsupported"

mag2 :: V2 -> Int
mag2 (V2 x y) = x * x + y * y

dist2 :: V2 -> V2 -> Int
dist2 p1 p2 = mag2 (p1 - p2)

dot :: V2 -> V2 -> Int
dot (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

cross :: V2 -> V2 -> Int
cross (V2 x1 y1) (V2 x2 y2) = x1 * y2 - x2 * y1

turn :: V2 -> V2 -> V2 -> Ordering
turn p1 p2 p3 = compare 0 (cross (p2 - p1) (p3 - p1))

{-# INLINABLE mag2 #-}
{-# INLINABLE dist2 #-}
{-# INLINABLE dot #-}
{-# INLINABLE cross #-}
{-# INLINABLE turn #-}

instance NFData V2 where
    rnf = rwhnf
