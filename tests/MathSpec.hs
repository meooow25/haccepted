{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
module MathSpec where

import Data.Array.Unboxed

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Math ( egcd, egcd2, mkFactorials, mkInvFactorials, mkBinom )
import MInt ( MInt )
import ArbitraryInstances ()

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

    describe "mkFactorials" $ do
        prop "Integer" $ testMkFactorials @Array @Integer
        prop "MInt" $ testMkFactorials @UArray @MInt

    describe "mkInvFactorials" $ do
        prop "Rational" $ testMkInvFactorials @Array @Rational
        prop "MInt" $ testMkInvFactorials @UArray @MInt

    describe "mkBinom" $ do
        prop "Rational" $ testMkBinom @Array @Rational
        prop "MInt" $ testMkBinom @UArray @MInt

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

    testMkFactorials :: forall a e. (Num e, Eq e, Show e, Arbitrary e, IArray a e)
                     => NonNegative Int -> Property
    testMkFactorials (NonNegative n) = do
        let fac = mkFactorials @a @e n
        forAll (choose (0, n)) $ \i ->
            fac!i `shouldBe` product (map fromIntegral [1..i])

    testMkInvFactorials :: forall a e. (Fractional e, Eq e, Show e, Arbitrary e, IArray a e)
                        => NonNegative Int -> Property
    testMkInvFactorials (NonNegative n) = do
        let fac = mkFactorials @a @e n
            ifac = mkInvFactorials @a @e n (recip (fac!n))
        forAll (choose (0, n)) $ \i ->
            ifac!i `shouldBe` recip (fac!i)

    testMkBinom :: forall a e. (Fractional e, Ord e, Show e, Arbitrary e, IArray a e)
                => NonNegative Int -> Property
    testMkBinom (NonNegative n) = do
        let binom = mkBinom @a @e n
            fact :: Int -> e
            fact i = product (map fromIntegral [1..i])
            naive n k | k < 0 || n < k = 0
            naive n k = fact n / (fact k * fact (n - k))
        forAll ((,) <$> choose (0, n) <*> arbitrary) $ \(n, k) ->
            binom n k `shouldBe` naive n k
