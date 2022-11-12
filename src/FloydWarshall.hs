{-# LANGUAGE FlexibleContexts #-}
{-|
Floyd-Warshall algorithm

Finds shortest paths between all pairs of vertices in a directed weighted graph.

Sources:
* https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
* Robert W. Floyd, "Algorithm 97: Shortest Path", Communications of the ACM, Vol. 5, No. 6, 1962
  https://dl.acm.org/doi/10.1145/367766.368168

Implementation notes:
* loop l r is used instead of for_ [l..r] because [l..r] gets let-floated out of the nested loops,
  preventing it from being optimizing away.

floydWarshallFromEdges
Runs the algorithm on the graph represented by the given list of edges. There should be no negative
cycles. O(|V|^3). Queries take O(1).

floydWarshall
Runs the algorithm on the adjacency matrix of the graph given as a mutable array. O(|V|^3). 
-}

module FloydWarshall
    ( WEdge(..)
    , Weight
    , floydWarshallFromEdges
    , floydWarshall
    ) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Graph

import Misc ( modifyArray )

type Weight = Int
data WEdge = WEdge !Vertex !Vertex !Weight deriving (Eq, Show)

floydWarshallFromEdges :: Bounds -> [WEdge] -> Vertex -> Vertex -> Maybe Weight
floydWarshallFromEdges (l, r) es = qry where
    qry u v = let x = da!(u, v) in x <$ guard (x < maxBound)
    da = runSTUArray $ do
        d <- newArray ((l, l), (r, r)) maxBound
        forM_ es $ \(WEdge u v w) -> modifyArray d (u, v) (min w)
        forM_ [l..r] $ \i -> writeArray d (i, i) 0
        floydWarshall d
        pure d

floydWarshall :: MArray a Weight m => a (Vertex, Vertex) Weight -> m ()
floydWarshall d = do
    ((l, _), (r, _)) <- getBounds d
    loop l r $ \k ->
        loop l r $ \i -> do
            ik <- readArray d (i, k)
            when (ik < maxBound) $ loop l r $ \j -> do
                kj <- readArray d (k, j)
                when (kj < maxBound) $
                    modifyArray d (i, j) (min (ik + kj))
  where
    loop l r f = go l where go i = when (i <= r) $ f i >> go (i + 1)

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE floydWarshall #-}
