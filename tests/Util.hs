module Util where

import Data.Array
import Data.Functor
import Data.Graph
import Test.QuickCheck

import Prufer ( seqToGraph )

genIntPair :: (Int, Int) -> Gen (Int, Int)
genIntPair bnds = (,) <$> choose bnds <*> choose bnds

genPruferSeq :: Gen ((Int, Int), [Int])
genPruferSeq = sized $ \n -> do
    l <- arbitrary
    let r = l + n - 1
    us <- drop 2 <$> shuffle [l .. r]
    return ((l, r), us)

genTreeG :: Gen Graph
genTreeG = uncurry seqToGraph <$> genPruferSeq

genTree :: Gen (Bounds, Tree Vertex)
genTree = do
    g <- genTreeG
    let bnds = bounds g
    x <- choose bnds
    let [t] = dfs g [x]
    return (bnds, t)

genForest :: Gen (Bounds, [Tree Vertex])
genForest = do
    ((l, r), t@(Node x _)) <- nonEmpty genTree
    let (Node _ ts) = t <&> \y -> if y > x then y - 1 else y
    return ((l, r - 1), ts)

nonEmpty :: Gen (Bounds, a) -> Gen (Bounds, a)
nonEmpty = (`suchThat` ok) where ok ((l, r), _) = l <= r
