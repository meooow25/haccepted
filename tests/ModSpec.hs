{-# LANGUAGE AllowAmbiguousTypes, DataKinds, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module ModSpec where

import Control.Monad
import Data.Proxy
import Data.Ratio
import Numeric.Natural
import GHC.TypeNats ( KnownNat, SomeNat(..), natVal, someNatVal )

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Math ( egcd )
import Mod ( Mod(..), invMaybe )

spec :: Spec
spec = do
    describe "add,sub,mul" $ do
        prop "Mod 1000000007 Int" $ testAddSubMul @1000000007 @Int
        prop "Mod 998244353 Int" $ testAddSubMul @998244353 @Int
        prop "Mod 7 Int" $ testAddSubMul @7 @Int
        prop "Mod 1000000007 Integer" $ testAddSubMul @1000000007 @Integer
        prop "Mod [any] Int" $
            forAll (arbitrary `suchThat` (>=2)) $ \m ->
                case someNatVal m of SomeNat (_ :: Proxy m) -> testAddSubMul @m @Int

    -- can't easily test add,sub together with div because of divide by 0 :(
    describe "mul,div" $ do
        prop "Mod 1000000007 Int" $ testMulDiv @1000000007 @Int
        prop "Mod 998244353 Int" $ testMulDiv @998244353 @Int
        prop "Mod 7 Int" $ testMulDiv @7 @Int
        prop "Mod 1000000007 Integer" $ testMulDiv @1000000007 @Integer
        prop "Mod [prime] Int" $
            forAll (arbitrary `suchThat` isPrime) $ \m ->
                case someNatVal m of SomeNat (_ :: Proxy m) -> testMulDiv @m @Int

    describe "recip" $ do
        prop "Mod 1000000007 Int" $ testRecip @1000000007 @Int
        prop "Mod 998244353 Int" $ testRecip @998244353 @Int
        prop "Mod 7 Int" $ testRecip @7 @Int
        prop "Mod 1000000007 Integer" $ testRecip @1000000007 @Integer
        prop "Mod [prime] Int" $
            forAll (arbitrary `suchThat` isPrime) $ \m ->
                case someNatVal m of SomeNat (_ :: Proxy m) -> testRecip @m @Int

    describe "invMaybe" $ do
        prop "Mod [any] Int" $
            forAll (arbitrary `suchThat` (>=2)) $ \m ->
                case someNatVal m of SomeNat (_ :: Proxy m) -> testInvMaybe @m @Int

  where
    testAddSubMul :: forall m i. (KnownNat m, Integral i, Show i) => Property
    testAddSubMul = testModOps @m @i arbitrary [(:+), (:-), (:*)]

    testMulDiv :: forall m i. (KnownNat m, Integral i, Show i) => Property
    testMulDiv = testModOps @m @i gen [(:*), (:/)] where
        gen = arbitrary `suchThat` (fromIntegral (natVal @m Proxy) `doesNotDivide`)

    testModOps :: forall m i. (KnownNat m, Integral i, Show i)
               => Gen Integer -> [Expr -> Expr -> Expr] -> Property
    testModOps = propOpsVsRational @(Mod m i) unMod (fromIntegral $ natVal @m Proxy)

    testRecip :: forall m i. (KnownNat m, Integral i, Show i) => Property
    testRecip = forAll (arbitrary `suchThat` (m `doesNotDivide`)) $ \x -> do
        let x' = fromIntegral (x :: Integer) :: Mod m i
        recip x' * x' `shouldBe` 1
      where
        m = fromIntegral (natVal @m Proxy)

    testInvMaybe :: forall m i. (KnownNat m, Integral i, Show i) => Property
    testInvMaybe = forAll (arbitrary `suchThat` (m `doesNotDivide`)) $ \x -> do
        let x' = fromIntegral (x :: Integer) :: Mod m i
        case invMaybe x' of
            Nothing -> gcd x m `shouldNotBe` 1
            Just y  -> do
                gcd x m `shouldBe` 1
                x' * y `shouldBe` 1
      where
        m = fromIntegral (natVal @m Proxy)

ratMod :: Integral i => i -> Rational -> Maybe i
ratMod m r
    | den == 0 = Nothing
    | (1, deninv) <- den `egcd` m' = Just $ fromIntegral $ numerator r * deninv `mod` m'
    | otherwise = error "ratMod: no inverse"
  where
    m' = fromIntegral m
    den = denominator r `mod` m'

propOpsVsRational
    :: forall a i. (Eq a, Fractional a, Show a, Eq i, Integral i, Show i)
    => (a -> i) -> i -> Gen Integer -> [Expr -> Expr -> Expr] -> Property
propOpsVsRational f2i m genInteger ops =
    forAll (genExpr genInteger ops) $ \expr -> do
        let x = evalExpr @a expr
            y = evalExpr @Rational expr
        f2i <$> x `shouldBe` (y >>= ratMod (fromIntegral m))

data Expr
    = Lit Integer
    | Expr :+ Expr
    | Expr :- Expr
    | Expr :* Expr
    | Expr :/ Expr
    deriving Show

genExpr :: Gen Integer -> [Expr -> Expr -> Expr] -> Gen Expr
genExpr genInteger ops = sized $ \n -> choose (1, max 1 n) >>= go where
    go sz | sz <= 0 = error "genExpr: size <= 0"
    go 1 = Lit <$> genInteger
    go sz = do
        lsz <- choose (1, sz-1)
        elements ops <*> go lsz <*> go (sz - lsz)

evalExpr :: (Eq a, Fractional a) => Expr -> Maybe a
evalExpr = \case
    Lit i  -> Just (fromInteger i)
    x :+ y -> (+) <$> evalExpr x <*> evalExpr y
    x :- y -> (-) <$> evalExpr x <*> evalExpr y
    x :* y -> (*) <$> evalExpr x <*> evalExpr y
    x :/ y -> do
        x' <- evalExpr x
        y' <- evalExpr y
        (x' / y') <$ guard (y' /= 0)

doesNotDivide :: Integral i => i -> i -> Bool
x `doesNotDivide` y = mod y x /= 0

isPrime :: Integral i => i -> Bool
isPrime x = x >= 2 && all (`doesNotDivide` x) (takeWhile (\y -> y*y <= x) [2..])

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
