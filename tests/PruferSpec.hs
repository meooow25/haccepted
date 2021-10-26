module PruferSpec where

import Data.Graph

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Prufer ( graphToSeq, seqToGraph )
import Util ( genPruferSeq )

spec :: Spec
spec = do
    it "graphToSeq small" $ do
        graphToSeq smallTree `shouldBe` [4, 4, 4, 5]
    it "seqToGraph small" $ do
        seqToGraph (1, 6) [4, 4, 4, 5] `shouldBe` smallTree
    prop "graphToSeq . seqToGraph == id" $
        forAll genPruferSeq $ \(bnds, us) -> do
            let g = seqToGraph bnds us
                us' = graphToSeq g
            us' `shouldBe` us

-- Example from Wiki page on Prufer seq
smallTree :: Graph
smallTree = buildG (1, 6)
    [ (1, 4), (4, 1)
    , (2, 4), (4, 2)
    , (3, 4), (4, 3)
    , (4, 5), (5, 4)
    , (5, 6), (6, 5)
    ]
