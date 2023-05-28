{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-|
Heavy-light decomposition

Decomposition of a tree of size n into multiple paths, such that there are O(log n) paths between
any pair of vertices.

HLD is useful for problems that have queries or updates on the path between two vertices.

This implementation maps each node to an integer position in [1..n] such that nodes in a path have
consecutive positions. A path between two vertices decomposes into O(log n) ranges of consecutive
integers, possibly simplifying the problem.

Sources:
* https://en.wikipedia.org/wiki/Heavy_path_decomposition
* https://cp-algorithms.com/graph/hld.html

Implementation notes:
* The size array need not be stored if subtree queries are not required

buildHLD
Builds the HLD structure from a tree. O(n).

posHLD
The position for the given vertex. O(1).

pathHLD
A list of position ranges which make up the path from u to v. O(log n).

edgePathHLD
pathsHLD but excludes the LCA. Useful when working with edges, each edge can be mapped to the node
it leads down into. O(log n).

subtreeHLD
A position range covering the subtree of the given node. O(1).

lcaHLD
The lowest common ancestor of two vertices. O(log n).
-}

module HLD
    ( HLD(..)
    , buildHLD
    , posHLD
    , pathHLD
    , edgePathHLD
    , subtreeHLD
    , lcaHLD
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Graph
import Data.Ord
import Data.Tree
import GHC.Exts

import Misc ( maximumByMaybe )

data HLD = HLD
    { par_ :: !(UArray Vertex Vertex)
    , dep_ :: !(UArray Vertex Int)
    , hed_ :: !(UArray Vertex Vertex)
    , pos_ :: !(UArray Vertex Int)
    , siz_ :: !(UArray Vertex Int)
    }

buildHLD :: Bounds -> Tree Vertex -> HLD
buildHLD bnds t = HLD par dep hed pos sz where
    [par, dep, sz] = runST $ do
        [pa, da, sa :: STUArray s Vertex Int] <- replicateM 3 $ newArray_ bnds
        let go (Node u ts) p d = do
                writeArray pa u p
                writeArray da u d :: ST s ()
                sm <- (1+) <$> foldr (\t1 k !acc -> go t1 u (d+1) >>= k . (acc+)) pure ts 0
                sm <$ writeArray sa u sm
        _ <- go t (rootLabel t) 0
        mapM unsafeFreeze [pa, da, sa]
    [hed, pos] = runST $ do
        [ha, xa :: STUArray s Vertex Int] <- replicateM 2 $ newArray_ bnds
        let go (Node u ts) h x = do
                writeArray ha u h
                writeArray xa u (x+1) :: ST s ()
                case maximumByMaybe (comparing ((sz!) . rootLabel)) ts of
                    Nothing -> pure (x+1)
                    Just heavy -> do
                        let lights = filter ((/= rootLabel heavy) . rootLabel) ts
                        x' <- go heavy h (x+1)
                        foldM (\x1 t1 -> go t1 (rootLabel t1) x1) x' lights
        _ <- go t (rootLabel t) 0
        mapM unsafeFreeze [ha, xa]

posHLD :: HLD -> Vertex -> Int
posHLD (HLD _ _ _ pos _) = (pos!)

pathHLD_ :: Bool -> HLD -> Vertex -> Vertex -> [(Int, Int)]
pathHLD_ keepLca (HLD par dep hed pos _) u0 v0 = build $ \c n ->
    let go u v
            | dep!hu > dep!hv = go v u
            | hu /= hv =
                let !xhv = pos!hv
                    !xv = pos!v
                in (xhv, xv) `c` go u (par!hv)
            | otherwise =
                let minmax x y = if x <= y then (x,y) else (y,x)
                    !(!xu, !xv) = minmax (pos!u) (pos!v)
                in case () of
                    _ | keepLca   -> (xu, xv) `c` n
                      | xu == xv  -> n
                      | otherwise -> (xu + 1, xv) `c` n
          where
            !hu = hed!u
            !hv = hed!v
    in go u0 v0
{-# INLINE pathHLD_ #-}

pathHLD :: HLD -> Vertex -> Vertex -> [(Int, Int)]
pathHLD = pathHLD_ True
{-# INLINE pathHLD #-}

edgePathHLD :: HLD -> Vertex -> Vertex -> [(Int, Int)]
edgePathHLD = pathHLD_ False
{-# INLINE edgePathHLD #-}

subtreeHLD :: HLD -> Vertex -> (Int, Int)
subtreeHLD (HLD _ _ _ pos sz) u = (pos!u, pos!u + sz!u - 1)

lcaHLD :: HLD -> Vertex -> Vertex -> Vertex
lcaHLD (HLD par dep hed pos _) = go where
    go u v
        | dep!hu > dep!hv = go v u
        | hu /= hv        = go u (par!hv)
        | otherwise       = let !xu = pos!u
                                !xv = pos!v
                            in if xu < xv then u else v
      where
        !hu = hed!u
        !hv = hed!v

--------------------------------------------------------------------------------
-- For tests

instance NFData HLD where
    rnf = rwhnf
