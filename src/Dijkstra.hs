{-# LANGUAGE ScopedTypeVariables #-}
{-|
Dijkstra's algorithm

An algorithm to find multi-source shortest paths in a graph with non-negative edges.

There are a variety of possible implementations depending on what is required, such as finding
parents, early stopping, etc. This is a basic implementation calculating only the distances,
to be modified when required.

Sources:
* Edgar W. Dijkstra, "A note on two problems in connexion with graphs", 1959
  https://www-m3.ma.tum.de/foswiki/pub/MN0506/WebHome/dijkstra.pdf
* Implementation is folklore

Implementation notes:
* dijkstra uses a Set as priority queue because there is no readily available priority queue
  structure in base.
* dijkstraH uses a skew heap as priority queue. Why a skew heap? Because it is pretty fast and easy
  to implement. An in-place binary heap would likely perform better, but hasn't been tested.

dijkstra
Runs Dijkstra's algorithm on the given graph. Unreachable vertices have distance maxBound.
O((V + E) log V).

dijkstraH
Runs Dijkstra's algorithm on the given graph. srcs should not have duplicates. Unreachable vertices
have distance maxBound. Faster than dijkstra, especially for large sparse graphs. O((V + E) log E).
-}

module Dijkstra
    ( dijkstra
    , dijkstraH
    , Weight
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Graph
import Data.List
import qualified Data.Set as S

import LabelledGraph ( LGraph )

type Weight = Int

dijkstra :: LGraph Weight -> [Vertex] -> UArray Vertex Weight
dijkstra g srcs = runSTUArray $ do
    let bnds = bounds g
    d :: STUArray s Vertex Weight <- newArray bnds maxBound
    let go = maybe (pure ()) go' . S.minView
        go' ((du, u), q) = foldM f q (g!u) >>= go where
            f q' (w, v) = do
                dv <- readArray d v
                let dv' = du + w
                if dv <= dv' then pure q' else do
                    writeArray d v dv' :: ST s ()
                    pure $ S.insert (dv', v) (S.delete (dv, v) q')
    forM_ srcs $ \v -> writeArray d v 0
    go (S.fromList ((,) 0 <$> srcs))
    pure d

dijkstraH :: LGraph Weight -> [Vertex] -> UArray Vertex Weight
dijkstraH g srcs = runSTUArray $ do
    let bnds = bounds g
    d :: STUArray s Vertex Weight <- newArray bnds maxBound
    let go Tip = pure ()
        go (Bin du u ql qr) = do
            du' <- readArray d u
            if du == du' then foldM f qlr (g!u) >>= go else go qlr
          where
            qlr = unionH ql qr
            f q (w, v) = do
                dv <- readArray d v
                let dv' = du + w
                if dv <= dv' then pure q else do
                    writeArray d v dv' :: ST s ()
                    pure $ unionH q (Bin dv' v Tip Tip)
    forM_ srcs $ \v -> writeArray d v 0
    go (foldl' unionH Tip [Bin 0 v Tip Tip | v <- srcs])
    pure d

data DHeap = Tip | Bin !Weight !Vertex !DHeap !DHeap

unionH :: DHeap -> DHeap -> DHeap
unionH Tip h2 = h2
unionH h1 Tip = h1
unionH h1@(Bin w1 v1 l1 r1) h2@(Bin w2 v2 l2 r2)
   | w1 <= w2  = Bin w1 v1 (unionH r1 h2) l1
   | otherwise = Bin w2 v2 (unionH r2 h1) l2
