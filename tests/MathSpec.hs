{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
module MathSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Math ( egcd, egcd2 )

spec :: Spec
spec = do
    describe "egcd2" $ do
        testEgcd2 @Int "Int"
        testEgcd2 @(Large Int) "Large Int"
        testEgcd2 @Integer "Integer"
        testEgcd2 @Word "Word"
    describe "egcd" $ do
        testEgcd @Int "Int"
        testEgcd @(Large Int) "Large Int"
        testEgcd @Integer "Integer"
        testEgcd @Word "Word"
  where
    testEgcd2 :: forall i. (Integral i, Show i, Arbitrary i) => String -> Spec
    testEgcd2 s = prop s $ \a b -> do
        let (g, _, _) = egcd2 @i (abs a) (abs b)
        g `shouldBe` gcd a b
        let (g', s, t) = egcd2 @i a b
        abs g' `shouldBe` g
        a * s + b * t `shouldBe` g'

    testEgcd :: forall i. (Integral i, Show i, Arbitrary i) => String -> Spec
    testEgcd s = prop s $ \a b -> do
        let (g, s) = egcd @i a b
            (g', s', _) = egcd2 @i a b
        (g, s) `shouldBe` (g', s')
