{-|
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
* Update the turn condition to ==GT to allow redundant points on the hull. Warning: this introduces
  a duplication issue when all points are collinear.

convexHull
Calculates the convex hull for a set of points. The points should be distinct. The points on the
hull are returned in clockwise order. O(n log n).
-}

module ConvexHull
    ( convexHull
    ) where

import Data.List

import Geometry ( V2, turn )

convexHull :: [V2] -> [V2]
convexHull []  = []
convexHull [p] = [p]
convexHull ps  = tail (foldl' f [] (reverse ps')) ++ tail (foldl' f [] ps') where
    ps' = sort ps
    f hull p = go hull where
        go (p1:h@(p2:_)) | turn p2 p1 p /= LT = go h
        go h = p:h
