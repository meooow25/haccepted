module PruferSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Prufer ( graphToSeq, seqToGraph )
import Util ( genPruferSeq )

spec :: Spec
spec = do
    prop "graphToSeq . seqToGraph == id" $
        forAll (scale (*10) genPruferSeq) $ \(bnds, us) -> do
            let g = seqToGraph bnds us
                us' = graphToSeq g
            us' `shouldBe` us

-- TODO: Add unit tests
