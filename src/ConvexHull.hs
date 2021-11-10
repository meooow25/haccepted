{-
2D convex hull

The convex hull is calculated using Andrew's monotone chain algorithm.

Sources:
* https://cp-algorithms.com/geometry/convex-hull.html
* R. L. Graham, "An Efficient Algorithm for Determining the Convex Hull of a Finite Planar Set",
  1972
  https://www.math.ucsd.edu/~ronspubs/72_10_convex_hull.pdf
* A. M. Andrew, "Another efficient algorithm for convex hulls in two dimensions", 1979
  https://www.sciencedirect.com/science/article/abs/pii/0020019079900723

Implementation notes:
* The description says points should be distinct but this is only an issue when the input is one
  point p repeated multiple times, the hull returned is [p, p] instead of [p].
* Update clockwise to < 0 to allow redundant points on the hull. Note that this introduces a
  duplication issue when all points are collinear.
* The runtime for sorted points is O(n) because Data.List.sort runs in O(n) for a sorted list,
  though this is not officially documented.

convexHull
Calculates the convex hull for a set of points. The points should be distinct. The points on the
hull are returned in clockwise order. O(n log n), but O(n) if the points are already sorted.
-}

module ConvexHull
    ( Point(..)
    , convexHull
    , clockwise
    ) where

import Control.DeepSeq
import Data.List

data Point a = Point !a !a deriving (Eq, Ord, Show)

convexHull :: (Num a, Ord a) => [Point a] -> [Point a]
convexHull []  = []
convexHull [p] = [p]
convexHull ps  = tail (go (reverse ps') []) ++ tail (go ps' []) where
    ps' = sort ps
    go ps@(p:_) (p1:h@(p2:_)) | clockwise p2 p1 p = go ps h
    go (p:ps) h = go ps (p:h)
    go []     h = h

clockwise :: (Num a, Ord a) => Point a -> Point a -> Point a -> Bool
clockwise (Point ax ay) (Point bx by) (Point cx cy) =
    (bx - ax) * (cy - by) - (by - ay) * (cx - bx) <= 0

--------------------------------------------------------------------------------
-- For tests

instance NFData a => NFData (Point a) where
    rnf (Point x y) = rnf x `seq` rnf y
