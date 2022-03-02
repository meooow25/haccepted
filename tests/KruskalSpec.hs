module KruskalSpec where

import Data.Graph
import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Kruskal ( WEdge(..), Weight, kruskal )

spec :: Spec
spec = do
    prop "kruskal" $
        forAll genEdges $ \(bnds, es) -> do
            let es' = kruskal bnds es
            es' `shouldSatisfy` isSpanningForest bnds es
            graphWeight es' `shouldBe` naiveMSFWeight bnds es

genEdges :: Gen (Bounds, [WEdge])
genEdges = sized $ \n -> do
    n' <- choose (0, n)
    NonNegative l <- arbitrary
    let bnds = (l, l + n' - 1)
        e = (WEdge <$> choose bnds <*> choose bnds <*> arbitrary)
            `suchThat` (\(WEdge u v _) -> u /= v)
    es <- if n' <= 1 then pure [] else listOf e `suchThat` ((<=10) . length)
    pure (bnds, es)

isSpanningForest :: Bounds -> [WEdge] -> [WEdge] -> Bool
isSpanningForest (l, r) es es' = isForest && noConnectableComps where
    comps = components $ buildG (l, r) [(u, v) | WEdge u v _ <- es']
    isForest = length es' + length comps == r - l + 1
    noConnectableComps = all bad (es \\ es') where
        bad (WEdge u v _) = getComp u == getComp v
        getComp u = findIndex (elem u) comps

graphWeight :: [WEdge] -> Weight
graphWeight = sum . map getW

naiveMSFWeight :: Bounds -> [WEdge] -> Weight
naiveMSFWeight bnds es =
    minimum $ map graphWeight $ filter (isSpanningForest bnds es) $ subsequences es
