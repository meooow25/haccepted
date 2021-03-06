module FenwickSpec where

import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Fenwick
    ( FTree
    , boundsF
    , emptyF
    , foldPrefixF
    , foldRangeF
    , fromListF
    , mappendF
    , mappendRangeF
    , toScanl1F
    )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    prop "updates, queries" $
        forAll genFt $ \ft -> do
            let (l, h) = boundsF ft
            forAll (pointUpds (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                forAll (pointQry (l, h)) $ \j ->
                    foldPrefixF j ft' `shouldBe` naive ivs l j

    prop "updates, range queries" $
        forAll genFt $ \ft -> do
            let (l, h) = boundsF ft
            forAll (pointUpds (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                forAll (genSortedIntPair (l, h)) $ \(i, j) ->
                    foldRangeF negate i j ft' `shouldBe` naive ivs i j

    prop "range updates, queries" $
        forAll genFt $ \ft -> do
            let (l, h) = boundsF ft
            forAll (rangeUpds (l, h)) $ \ijvs -> do
                let ft' = applyRangeUpdates ijvs ft
                    ivs = do
                        (i, j, v) <- ijvs
                        (i, v) : [(j + 1, -v) | j < h]
                forAll (pointQry (l, h)) $ \i ->
                    foldPrefixF i ft' `shouldBe` naive ivs l i

    prop "fromListF" $
        forAll genFt $ \ft -> do
            let (l, h) = boundsF ft
                n = h - l + 1
                v = vector n :: Gen [Int]
            forAll (map Sum <$> v) $ \xs -> do
                let ft' = fromListF (l, h) xs
                toScanl1F ft' `shouldBe` scanl1 (<>) xs

    prop "toScanl1F" $ \xs' -> do
            let xs = map Sum xs' :: [Sum Int]
                n = length xs
                ft = fromListF (1, n) xs
            toScanl1F ft `shouldBe` scanl1 (<>) xs

  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    applyUpdates ivs ft = foldl' (\ft (i, v) -> mappendF v i ft) ft ivs
    applyRangeUpdates ijvs ft = foldl' (\ft (i, j, v) -> mappendRangeF negate v i j ft) ft ijvs

genFt :: Gen (FTree (Sum Int))
genFt = sized $ \n -> do
    n' <- choose (0, n)
    l <- arbitrary
    pure $ emptyF (l, l + n' - 1)

pointUpds :: (Int, Int) -> Gen [(Int, Sum Int)]
pointUpds (l, h)
    | l == h + 1 = pure []
    | otherwise = listOf1 $ do
        i <- choose (l, h)
        v <- arbitrary
        pure (i, Sum v)

rangeUpds :: (Int, Int) -> Gen [(Int, Int, Sum Int)]
rangeUpds (l, h)
    | l == h + 1 = pure []
    | otherwise = listOf1 $ do
        (i, j) <- genSortedIntPair (l, h)
        v <- arbitrary
        pure (i, j, Sum v)

pointQry :: (Int, Int) -> Gen Int
pointQry (l, r) = chooseInt (l - 10, r + 10)
