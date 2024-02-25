{-# LANGUAGE BangPatterns #-}
{-|
Tree reroot fold

Folds of a tree with every node as root.
Known as "tree rerooting DP", among other names.

Recall that Data.Tree has foldTree :: (a -> [c] -> c) -> Tree a -> c. foldReroot is similar but
requires (b -> c -> b) and b to perform strict folds over [c].

g :: b -> c -> b must be commutative, in the sense that
(b `g` c1) `g` c2 = (b `g` c2) `g` c1

Sources:
* pajenegod, "The Ultimate Reroot Template"
  https://codeforces.com/blog/entry/124286

Implementation notes:
* Thanks to Haskell's laziness it is possible to write a more concise implementation with a single
  dfs, but this seems to be slower in practice. Here is a possible implementation:
  https://gist.github.com/meooow25/6460e45327355106cedbcf3bd5166cd6
* Reroot problems can often be solved in O(n) time, if there is a way to *take away* the
  contribution of a node from an accumulated value. Avoiding this requirement, it takes O(n log n)
  but is far simpler.

foldReroot
Returns the same tree with each vertex accompanied by the fold of the tree if that vertex is made
the root of the tree. f is called O(n) times. g is called O(n log n) times.
-}

module RerootFold
    ( foldReroot
    ) where

import Data.Tree
import Data.List

import Misc ( foldExclusive )

foldReroot :: (a -> b -> c) -> (b -> c -> b) -> b -> Tree a -> Tree (a, c)
foldReroot f g y0 t = res where
    res = Node (x,z) (zipWith (go2 . f x) ys ts) where
        !(z, Node (x,zs) ts) = go1 t
        ys = foldExclusive g y0 zs
    go1 (Node x ts) = (z, Node (x,zs) ts') where
        (zs, ts') = unzip (map go1 ts)
        !z = f x (foldl' g y0 zs)
    go2 !up (Node (x,zs) ts) = Node (x,z) (zipWith (go2 . f x) ys ts) where
        y:ys = foldExclusive g y0 (up:zs)
        !z = f x (g y up)
