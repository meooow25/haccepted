module CentroidDecompSpec where

import Data.Foldable
import Data.List
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
                nodeIsMadeUpOfChildren (t, ts) = sort us == sort us' where
                    us = toList t
                    us' = rootLabel t : concatMap toList ts

                visit (Node t tts) = do
                    -- sanity check, nodes are from the original tree
                    t `shouldSatisfy` (all (`elem` us) . toList)

                    -- the root is the centroid in its tree
                    t `shouldSatisfy` rootIsCentroid

                    -- the tree node in the centroid tree is made up of the nodes in its children
                    -- trees, plus the root.
                    (t, map rootLabel tts) `shouldSatisfy` nodeIsMadeUpOfChildren

                    mapM_ visit tts

            visit $ centroidDecompose t
