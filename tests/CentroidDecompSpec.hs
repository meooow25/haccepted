{-# LANGUAGE TupleSections #-}
module CentroidDecompSpec where

import Data.Foldable
import Data.Function
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Tree

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CentroidDecomp ( centroidDecompose, centroidDecomposeL )
import LabelledGraph ( LTree(..), lTreeToTree )
import Util ( genLTree, genTree )

spec :: Spec
spec = do
    let rootIsCentroid :: Foldable t => (t a -> [t a]) -> t a -> Bool
        rootIsCentroid getCh t = not $ any ((>sz) . (*2)) chSzs where
            treeSize = length . toList
            sz = treeSize t
            chSzs = map treeSize $ getCh t

    prop "centroidDecompose" $
        forAll (snd <$> genTree) $ \t -> do
            let us = sort $ toList t
                nodeIsMadeUpOfChildren t ts = t `eqTree` Node (rootLabel t) ts' where
                    nodeToSubtree = concatMap (\t -> map (,t) $ toList t) ts
                    getSubtree v = rerootTree v $ fromJust $ lookup v nodeToSubtree
                    ts' = map (getSubtree . rootLabel) $ subForest t

                visit (Node t tss) = do
                    -- sanity check, nodes are from the original tree
                    t `shouldSatisfy` (all (`elem` us) . toList)

                    -- the root is the centroid in its tree
                    t `shouldSatisfy` rootIsCentroid subForest

                    -- the tree node in the centroid tree is made up of its children trees connected
                    -- by the root
                    (t, map rootLabel tss) `shouldSatisfy` uncurry nodeIsMadeUpOfChildren

                    mapM_ visit tss

            visit $ centroidDecompose t

    -- same as centroidDecompose, for labelled graphs
    prop "centroidDecomposeL" $
        forAll (snd <$> genLTree) $ \t -> do
            let us = sort $ toList t
                nodeIsMadeUpOfChildren t ts = tWoLabels `eqLTree` LNode (rootLabelL t) ts' where
                    nodeToSubtree = concatMap (\t -> map (,t) $ toList t) ts
                    getSubtree v = rerootLTree v $ fromJust $ lookup v nodeToSubtree
                    -- labels to children of the centroid are not preserved in the centroid tree
                    tWoLabels = LNode (rootLabelL t) $ map (first $ const 0) $ subForestL t
                    ts' = map ((0,) . getSubtree . rootLabelL . snd) $ subForestL t

                visit (Node t tss) = do
                    -- sanity check, nodes are from the original tree
                    t `shouldSatisfy` (all (`elem` us) . toList)

                    -- the root is the centroid in its tree
                    t `shouldSatisfy` rootIsCentroid (map snd . subForestL)

                    -- the tree node in the centroid tree is made up of its children trees connected
                    -- by the root
                    (t, map rootLabel tss) `shouldSatisfy` uncurry nodeIsMadeUpOfChildren

                    mapM_ visit tss

            visit $ centroidDecomposeL t

    prop "centroidDecompose and centroidDecomposeL generate identical decompositions" $
        forAll (snd <$> genLTree) $ \t ->
            fmap lTreeToTree (centroidDecomposeL t) `shouldBe` centroidDecompose (lTreeToTree t)

eqTree :: Ord a => Tree a -> Tree a -> Bool
eqTree = (==) `on` sortTree where
    sortTree (Node u ts) = Node u $ map sortTree $ sortOn rootLabel ts

rerootTree :: Eq a => a -> Tree a -> Tree a
rerootTree r t = fromJust $ go t [] where
    go (Node u ts) par
        | u == r    = Just $ Node u $ par ++ ts
        | otherwise = asum $ zipWith3 f (inits ts) ts (tail $ tails ts)
      where
        f left t right = go t [Node u $ par ++ left ++ right]

eqLTree :: (Ord a, Ord b) => LTree b a -> LTree b a -> Bool
eqLTree = (==) `on` sortTree where
    sortTree (LNode u ts) = LNode u $ map (second sortTree) $ sortOn (rootLabelL . snd) ts

rerootLTree :: Eq a => a -> LTree b a -> LTree b a
rerootLTree r t = fromJust $ go t [] where
    go (LNode u ts) par
        | u == r    = Just $ LNode u $ par ++ ts
        | otherwise = asum $ zipWith3 f (inits ts) ts (tail $ tails ts)
      where
        f left (l, t) right = go t [(l, LNode u $ par ++ left ++ right)]
