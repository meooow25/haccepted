module HLDSpec where

import Data.Array.Unboxed
import Data.Foldable
import Data.Graph
import Data.List
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import HLD ( HLD(..), buildHLD, edgePathHLD, lcaHLD, pathHLD, subtreeHLD )
import LCASpec ( naiveLCA )
import Util ( genIntPair, genTree )

spec :: Spec
spec = do
    prop "positions are [1..n]" $
        forAll genTree $ \(bnds, t) -> do
            let n = rangeSize bnds
                hld = buildHLD bnds t
            sort (elems (pos_ hld)) `shouldBe` [1..n]

    prop "pathHLD" $ 
        forAll genTree $ \(bnds, t) ->
            forAll (genIntPair bnds) $ \(u,v) -> do
                let hld = buildHLD bnds t
                    path = map (posRevMap hld !) $ expand $ pathHLD hld u v
                sort path `shouldBe` sort (naivePath t u v)

    prop "edgePathHLD" $
        forAll genTree $ \(bnds, t) ->
            forAll (genIntPair bnds) $ \(u,v) -> do
                let hld = buildHLD bnds t
                    path = map (posRevMap hld !) $ expand $ edgePathHLD hld u v
                sort path `shouldBe` sort (tail $ naivePath t u v)

    prop "subtreeHLD" $
        forAll genTree $ \(bnds, t) ->
            forAll (choose bnds) $ \u -> do
                let hld = buildHLD bnds t
                    subtree = map (posRevMap hld !) $ expand [subtreeHLD hld u]
                sort subtree `shouldBe` sort (naiveSubtree u t)

    prop "hldLCA" $
        forAll genTree $ \(bnds, t) ->
            forAll (genIntPair bnds) $ \(u, v) -> do
                let hld = buildHLD bnds t
                lcaHLD hld u v `shouldBe` fromJust (naiveLCA u v [t])

expand :: [(Int, Int)] -> [Int]
expand = concatMap (uncurry enumFromTo)

posRevMap :: HLD -> UArray Int Vertex
posRevMap hld = array (1, r-l+1) [(x,i) | (i,x) <- assocs (pos_ hld)] where
    (l,r) = bounds (pos_ hld)

naivePath :: Tree Vertex -> Vertex -> Vertex -> [Vertex]
naivePath t u v = [lca] ++ drop (length same) pu ++ drop (length same) pv where
    pu = pathFromRoot u t
    pv = pathFromRoot v t
    same = takeWhile (uncurry (==)) $ zip pu pv
    (lca, _) = last same

naiveSubtree :: Vertex -> Tree Vertex -> [Vertex]
naiveSubtree v = fromJust . go where
    go t@(Node u ts)
        | u == v    = Just (toList t)
        | otherwise = asum (map go ts)

pathFromRoot :: Vertex -> Tree Vertex -> [Vertex]
pathFromRoot v = fromJust . go where
    go (Node u ts)
        | u == v    = Just [u]
        | otherwise = (u:) <$> asum (map go ts)
