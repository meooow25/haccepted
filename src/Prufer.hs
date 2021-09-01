{-# LANGUAGE ScopedTypeVariables #-}
{-
Prufer sequences

Functions to convert trees to Prufer sequences and vice versa

Sources:
* https://en.wikipedia.org/wiki/Pr%C3%BCfer_sequence
* Xiaodong Wang, Lei Wang, Yingjie Wu, "An Optimal Algorithm for Prufer Codes", 2009
  https://www.scirp.org/pdf/JSEA20090200006_93737200.pdf

graphToSeq
Convert a bidirected Graph to a Prufer sequence. The graph must be a connected tree or empty. O(n).

treeToSeq
Convert a Tree to a Prufer sequence. The tree must be rooted at r. O(n).

seqToGraph
Convert a Prufer sequence to a bidirected Graph. The sequence must be valid. O(n).

seqToEdges
Convert a Prufer sequence to a list of edges, each edge present twice in its two orientations. The
sequence must be valid. O(n).

Implementation note:
The c array tracks the number of children of a node.
-}
module Prufer
    ( graphToSeq
    , treeToSeq
    , seqToEdges
    , seqToGraph
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Graph

nxtLeaf :: STUArray s Vertex Int -> Vertex -> ST s Vertex
nxtLeaf c u = do
    cu <- readArray c u
    if cu == 0 then return u else nxtLeaf c $ u + 1

withSizeCheck :: Vertex -> Vertex -> [a] -> [a]
withSizeCheck l r xs
    | l > r + 1 = error "negative node count"
    | l >= r    = []
    | otherwise = xs

graphToSeq :: Graph -> [Vertex]
graphToSeq g = treeToSeq b t where
    b@(_, r) = bounds g
    [t] = dfs g [r]

treeToSeq :: Bounds -> Tree Vertex -> [Vertex]
treeToSeq (l, r) t = withSizeCheck l r $ runST $ do
    c :: STUArray s Vertex Int <- newArray (l, r) 0
    let setC (Node x ts) = writeArray c x (length ts) >> mapM_ setC ts :: ST s ()
        par = array (l, r) $ go r t [] :: UArray Vertex Vertex where
            go p (Node u ts) acc = (u, p) : foldr (go u) acc ts
        go k u = do
            let p = par!u
            cp <- readArray c p
            if p == r && cp == 1 then return [] else do
                writeArray c p $ cp - 1
                (p:) <$> if p < k && cp == 1
                            then go k p
                            else nxtLeaf c (k + 1) >>= join go
    setC t >> nxtLeaf c l >>= join go

seqToGraph :: Bounds -> [Vertex] -> Graph
seqToGraph bnds = buildG bnds . seqToEdges bnds

seqToEdges :: Bounds -> [Vertex] -> [Edge]
seqToEdges (l, r) ps = withSizeCheck l r $ runST $ do
    c :: STUArray s Vertex Int <- newArray (l, r) 0
    forM_ ps $ \u -> writeArray c u . (+1) =<< readArray c u
    let go [] _ u = return [(u, r), (r, u)]
        go (p:ps) k u = do
            cp <- readArray c p
            writeArray c p $ cp - 1
            ((u, p):) . ((p, u):) <$> if p < k && cp == 1
                                        then go ps k p
                                        else nxtLeaf c (k + 1) >>= join (go ps)
    nxtLeaf c l >>= join (go ps)
