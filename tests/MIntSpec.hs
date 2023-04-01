{-# LANGUAGE TypeApplications #-}
module MIntSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import MInt ( MInt(..), mm )

import ModSpec ( Expr(..), doesNotDivide, propOpsVsRational )

spec :: Spec
spec = do
    prop "add,sub,mul" $
        testModOps arbitrary [(:+), (:-), (:*)]

    -- can't easily test add,sub together with div because of divide by 0 :(
    prop "mul,div" $
        testModOps (arbitrary `suchThat` (fromIntegral mm `doesNotDivide`)) [(:*), (:/)]

    prop "recip" $
        forAll (arbitrary `suchThat` (mm `doesNotDivide`)) $ \x -> do
            let x' = fromIntegral x :: MInt
            recip x' * x' `shouldBe` 1
  where
    testModOps :: Gen Integer -> [Expr -> Expr -> Expr] -> Property
    testModOps = propOpsVsRational @MInt unMInt mm
