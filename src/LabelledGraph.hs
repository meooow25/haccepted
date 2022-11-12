{-# LANGUAGE DeriveTraversable #-}
{-|
Utilities for working with edge-labeled/weighted graphs

There are useful definitions and algorithms in Data.Tree and Data.Graph but sadly these only deal
with unlabeled graphs.
Most definitions here mirror those in Data.Graph.

buildLG
Builds a LGraph from a list of LEdges. O(n + m) for bounds size n and m edges.

dfsLTree
For a LGraph that is known to be a tree, returns its LTree representation. O(n).

lTreeToTree
Drops labels from an LTree. O(n).
-}

module LabelledGraph
    ( LEdge
    , LGraph
    , LTree(..)
    , buildLG
    , dfsLTree
    , lTreeToTree
    ) where

import Data.Array
import Data.Graph
import Control.DeepSeq

type LEdge b = (Vertex, (b, Vertex))

type LGraph b = Array Vertex [(b, Vertex)]

data LTree b a = LNode
    { rootLabelL :: a
    , subForestL :: [(b, LTree b a)]
    } deriving (Eq, Show, Functor, Foldable, Traversable)

buildLG :: Bounds -> [LEdge b] -> LGraph b
buildLG = accumArray (flip (:)) []

dfsLTree :: LGraph b -> Vertex -> LTree b Vertex
dfsLTree g u = go u u where
    go p u = LNode u [(l, go u v) | (l, v) <- g!u, v /= p]

lTreeToTree :: LTree b a -> Tree a
lTreeToTree (LNode a ts) = Node a $ map (lTreeToTree . snd) ts

--------------------------------------------------------------------------------
-- For tests

instance (NFData a, NFData b) => NFData (LTree b a) where
    rnf (LNode u ts) = rnf u `seq` rnf ts
