{-# LANGUAGE ScopedTypeVariables #-}
{-
Dinic's algorithm or Dinitz's algorithm

An algorithm to find the maximum flow in a flow network.

Sources:
* Y. Dinitz, "Dinitz' Algorithm: The Original Version and Even's Version", 2006
  https://www.cs.bgu.ac.il/~dinitz/Papers/Dinitz_alg.pdf
* AC Library
  https://github.com/atcoder/ac-library/blob/master/atcoder/maxflow.hpp

Implementation notes:
* This implementation is close to B. Cherkassky's implementation in Dinitz's paper above, and the
  implementation in AC Library.
* The flow array contains flows of edges in the residual graph, forward and backward edges occupy
  even and odd positions.

dinic
Runs Dinic's algorithm on the graph made up of the given FlowEdges. Returns a FlowResult, describing
a max flow configuration with
1. the max flow value
2. flow values of the edges in the order in which they were given
3. a list of Bools for each edge indicating whether the edge is in a min-cut
O(V^2 E) in the general case.
O(min(V^(2/3), E^(1/2)) E) for unit capacity graphs.
O(V^(1/2) E) for unit networks, such as in maximum bipartite matching.
-}

module Dinic
    ( Flow
    , FlowEdge(..)
    , FlowResult(..)
    , ToEdge(..)
    , dinic
    ) where

import Control.DeepSeq
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Bits
import Data.Graph

import Misc ( modifyArray )

type Flow = Int
data FlowEdge = FlowEdge { from_ :: !Vertex, to_ :: !Vertex, cap_ :: !Flow } deriving (Eq, Show)

type EdgeIndex = Int
data ToEdge = ToEdge { to__ :: !Vertex, edgeIndex_ :: !EdgeIndex }

data FlowResult
    = FlowResult
    { getFlow :: !Flow
    , getEdgeFlows :: [Flow]
    , getMinCut :: [Bool]
    }

dinic :: Bounds -> [FlowEdge] -> Vertex -> Vertex -> FlowResult
dinic _ _ src sink | src == sink = error "src == sink"
dinic bnds es src sink = FlowResult maxFlow flows minCut where
    m = length es
    g :: Array Vertex [ToEdge] = accumArray (flip (:)) [] bnds $ do
        (i, FlowEdge u v _) <- zip [0..] es
        [(u, ToEdge v (2*i)), (v, ToEdge u (2*i+1))]
    flows = map (flowa!) [0,2..2*m-2]
    minCut = [lvl!u == -1 && lvl!v /= -1 | FlowEdge u v _ <- es] where
        lvl = runST $ dinicLevels (pure . (==0) . (flowa!))

    (maxFlow, flowa :: UArray Int Flow) = runST $ do
        flow :: STUArray s EdgeIndex Flow <-
            newListArray (0, 2*m-1) $ concat [[0, c] | FlowEdge _ _ c <- es]
        let runDinic = do
                lvl <- dinicLevels (fmap (==0) . readArray flow)
                f <- dinicAugment (\u v -> lvl!v == lvl!u - 1) flow
                if f == 0 then pure 0 else (f+) <$> runDinic
        (,) <$> runDinic <*> unsafeFreeze flow

    dinicLevels :: forall s. (EdgeIndex -> ST s Bool) -> ST s (UArray Vertex Int)
    dinicLevels sat = do
        lvl :: STUArray s Vertex Int <- newArray (bounds g) (-1)
        let visit _ [] = pure ()
            visit d q = go [] q [] >>= visit (d + 1) where
                go [] []     acc = pure acc
                go [] (u:us) acc = go (g!u) us acc
                go (ToEdge v i:es) us acc = do
                    l <- readArray lvl v
                    s <- sat i
                    if l /= -1 || s
                        then go es us acc
                        else do
                            writeArray lvl v d
                            if v == src then pure [] else go es us (v:acc)
        writeArray lvl sink 0
        visit 1 [sink]
        unsafeFreeze lvl
    {-# INLINE dinicLevels #-}

    dinicAugment :: forall s. (Vertex -> Vertex -> Bool) -> STUArray s EdgeIndex Flow -> ST s Flow
    dinicAugment nxtLvl flow = do
        g' :: STArray s Vertex [ToEdge] <- thaw g
        let go _ 0 = pure 0
            go u fup | u == sink = pure fup
            go u fup = readArray g' u >>= go' where
                go' [] = pure 0 :: ST s Flow
                go' (ToEdge v _:rest) | not (nxtLvl u v) = writeArray g' u rest >> go u fup
                go' (ToEdge v i:rest) = do
                    f <- readArray flow (xor i 1)
                    fdn <- go v (min fup f)
                    modifyArray flow i (+fdn) 
                    modifyArray flow (xor i 1) (+(-fdn))
                    if fdn == fup
                        then pure fdn
                        else writeArray g' u rest >> (fdn+) <$> go u (fup - fdn)
        go src maxBound
    {-# INLINE dinicAugment #-}

--------------------------------------------------------------------------------
-- For tests

instance NFData FlowEdge where
    rnf = rwhnf
