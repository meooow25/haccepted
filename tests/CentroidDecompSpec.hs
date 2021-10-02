module CentroidDecompSpec where

import Control.Monad
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
                ts = centroidDecompose t

            forM_ ts $ \t -> do
                -- sanity check, nodes are from the original tree
                t `shouldSatisfy` (all (`elem` us) . toList)

                -- the root is the centroid
                let rootIsCentroid t = not $ any ((>sz) . (*2)) chSzs where
                        treeSize = length . toList
                        sz = treeSize t
                        chSzs = map treeSize $ subForest t
                t `shouldSatisfy` rootIsCentroid

            -- every node only appears exactly once as root
            sort (map rootLabel ts) `shouldBe` us
