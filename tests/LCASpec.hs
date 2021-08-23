module LCASpec where

import Control.Applicative
import Control.Exception
import Data.Foldable
import Data.Graph

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import LCA ( buildLCA, query1LCA, queryLCA )
import Util ( genIntPair, genForest, nonEmpty )

spec :: Spec
spec = do
    prop "queryLCA" $
        forAll (scale (*10) $ nonEmpty genForest) $ \(bnds, ts) -> do
            let lca = buildLCA bnds ts
            forAll (genIntPair bnds) $ \(u, v) -> do
                let l = queryLCA u v lca
                case naiveLCA u v ts of
                    Nothing -> evaluate l `shouldThrow` anyException
                    Just l' -> l `shouldBe` l'

    prop "query1LCA" $
        forAll (scale (*10) $ nonEmpty genForest) $ \(bnds, ts) -> do
            let lca = buildLCA bnds ts
            forAll (genIntPair bnds) $ \(u, v) ->
                query1LCA u v lca `shouldBe` naiveLCA u v ts

naiveLCA :: Vertex -> Vertex -> [Tree Vertex] -> Maybe Vertex
naiveLCA u v ts = asum $ map go ts where
    go t@(Node x ts) = asum (map go ts) <|> me where
        nodes = toList t
        me = if u `elem` nodes && v `elem` nodes then Just x else Nothing
