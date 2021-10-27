{-# LANGUAGE ScopedTypeVariables #-}
{-
Breadth first search

Source:
* https://en.wikipedia.org/wiki/Breadth-first_search

bfs
BFS on a graph, starting from the given source vertices. One tree per source is returned, which
contains all the vertices reached from the source before they could be reached by another.
Note that this is unlike Data.Graph.dfs, which returns one Tree for each connected component.
O(n + m), for a graph with n vertices and m edges.
-}

module BFS
    ( bfs
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Graph
import qualified Data.Sequence as Seq

import Misc ( modifyArray )

bfs :: Graph -> [Vertex] -> Forest Vertex
bfs g vs = map toTree vs where
    bnds = bounds g
    g' = runSTArray $ do
        vis :: STUArray s Vertex Bool <- newArray bnds False
        ch :: STArray s Vertex [Vertex] <- newArray bnds []
        let go Seq.Empty = pure ()
            go (u Seq.:<| q) = foldM f q (g!u) >>= go where
                f q v = readArray vis v >>= \m -> if m then pure q else add q v
                add q v = do
                    writeArray vis v True
                    modifyArray ch u (v:)
                    pure $ q Seq.|> v :: ST s (Seq.Seq Vertex)
        forM_ vs $ \v -> writeArray vis v True
        go $ Seq.fromList vs
        pure ch
    toTree u = Node u $ map toTree (g'!u)
