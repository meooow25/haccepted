{-# LANGUAGE TupleSections #-}
module CentroidDecompSpec where

import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Tree

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CentroidDecomp ( centroidDecompose )
import Util ( genTree, nonEmpty )

spec :: Spec
spec = do
    prop "centroidDecompose" $
        forAll (snd <$> nonEmpty genTree) $ \t -> do
            let us = sort $ toList t
                rootIsCentroid t = not $ any ((>sz) . (*2)) chSzs where
                    treeSize = length . toList
                    sz = treeSize t
                    chSzs = map treeSize $ subForest t
                nodeIsMadeUpOfChildren t ts = t `treeEq` Node (rootLabel t) ts' where
                    nodeToSubtree = concatMap (\t -> map (,t) $ toList t) ts
                    getSubtree v = reroot v $ fromJust $ lookup v nodeToSubtree
                    ts' = map (getSubtree . rootLabel) $ subForest t

                visit (Node t tss) = do
                    -- sanity check, nodes are from the original tree
                    t `shouldSatisfy` (all (`elem` us) . toList)

                    -- the root is the centroid in its tree
                    t `shouldSatisfy` rootIsCentroid

                    -- the tree node in the centroid tree is made up of its children trees connected
                    -- by the root
                    (t, map rootLabel tss) `shouldSatisfy` uncurry nodeIsMadeUpOfChildren

                    mapM_ visit tss

            visit $ centroidDecompose t

treeEq :: Ord a => Tree a -> Tree a -> Bool
treeEq = (==) `on` sortTree where
    sortTree (Node u ts) = Node u $ map sortTree $ sortOn rootLabel ts

reroot :: Eq a => a -> Tree a -> Tree a
reroot r t = fromJust $ go t [] where
    go (Node u ts) par
        | u == r    = Just $ Node u $ par ++ ts
        | otherwise = asum $ zipWith3 f (inits ts) ts (tail $ tails ts)
      where
        f left t right = go t [Node u $ par ++ left ++ right]
