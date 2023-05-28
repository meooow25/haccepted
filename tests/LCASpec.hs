module LCASpec where

import Control.Applicative
import Data.Foldable
import Data.Graph
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import LCA ( build1LCA, buildLCA, query1LCA, queryLCA )
import Util ( genIntPair, genNonEmptyForest, genTree )

spec :: Spec
spec = do
    prop "LCA on tree" $
        forAll genTree $ \(bnds, t) -> do
            let lca = buildLCA bnds t
            forAll (genIntPair bnds) $ \(u, v) -> do
                queryLCA u v lca `shouldBe` fromJust (naiveLCA u v [t])

    prop "LCA on forest" $
        forAll genNonEmptyForest $ \(bnds, ts) -> do
            let lca = build1LCA bnds ts
            forAll (genIntPair bnds) $ \(u, v) ->
                query1LCA u v lca `shouldBe` naiveLCA u v ts

naiveLCA :: Vertex -> Vertex -> [Tree Vertex] -> Maybe Vertex
naiveLCA u v ts = asum $ map go ts where
    go t@(Node x ts) = asum (map go ts) <|> me where
        me = if u `elem` t && v `elem` t then Just x else Nothing
