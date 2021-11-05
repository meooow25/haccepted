module Util where

import Data.Array
import Data.Functor
import Data.Graph

import Test.QuickCheck

import LabelledGraph ( LGraph, LTree, buildLG, dfsLTree )
import Prufer ( seqToGraph )

genIntPair :: (Int, Int) -> Gen (Int, Int)
genIntPair bnds = (,) <$> choose bnds <*> choose bnds

genSortedIntPair :: (Int, Int) -> Gen (Int, Int)
genSortedIntPair = fmap minmax . genIntPair where
    minmax (x, y) = (min x y, max x y)

genPossiblyEmptyRange :: (Int, Int) -> Gen (Int, Int)
genPossiblyEmptyRange (l, r) = do
    (x, y) <- genSortedIntPair (l, r + 1)
    pure (x, y - 1)

genPruferSeq :: Gen ((Int, Int), [Int])
genPruferSeq = sized $ \n -> do
    l <- arbitrary
    let r = l + n - 1
    us <- drop 2 <$> shuffle [l..r]
    return ((l, r), us)

genTreeG :: Gen Graph
genTreeG = uncurry seqToGraph <$> genPruferSeq

genTree :: Gen (Bounds, Tree Vertex)
genTree = do
    g <- genTreeG `suchThat` ((>0) . rangeSize . bounds)
    let bnds = bounds g
    x <- choose bnds
    let [t] = dfs g [x]
    return (bnds, t)

genForest :: Gen (Bounds, [Tree Vertex])
genForest = do
    ((l, r), t@(Node x _)) <- genTree
    let (Node _ ts) = t <&> \y -> if y > x then y - 1 else y
    return ((l, r - 1), ts)

genLTreeG :: Gen (LGraph Int)
genLTreeG = do
    g <- genTreeG
    let es = edges g
    ls <- vector (length es) :: Gen [Int]
    pure $ buildLG (bounds g) [(u, (l, v)) | ((u, v), l) <- zip es ls]

genLTree :: Gen (Bounds, LTree Int Vertex)
genLTree = do
    g <- genLTreeG `suchThat` ((>0) . rangeSize . bounds)
    let bnds = bounds g
    x <- choose bnds
    let t = dfsLTree g x
    pure (bnds, t)

genNonEmptyForest :: Gen (Bounds, [Tree Vertex])
genNonEmptyForest = genForest `suchThat` ((>0) . rangeSize . fst)
