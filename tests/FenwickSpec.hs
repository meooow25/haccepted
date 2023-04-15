{-# LANGUAGE ScopedTypeVariables #-}
module FenwickSpec where

import Data.Foldable
import Data.Maybe
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Fenwick
    ( FTree
    , binSearchF
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
        forAll genBounds $ \bnds@(l,_) ->
            forAll (pointUpds bnds) $ \ivs -> do
                let ft = applyUpdates ivs (emptyF bnds)
                forAll (pointQry bnds) $ \j ->
                    foldPrefixF j ft `shouldBe` naive ivs l j

    prop "updates, range queries" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                let ft = applyUpdates ivs (emptyF bnds)
                forAll (genSortedIntPair bnds) $ \(i, j) ->
                    foldRangeF i j ft `shouldBe` naive ivs i j

    prop "range updates, queries" $
        forAll genBounds $ \bnds@(l,h) ->
            forAll (rangeUpds bnds) $ \ijvs -> do
                let ft = applyRangeUpdates ijvs (emptyF bnds)
                    ivs = do
                        (i, j, v) <- ijvs
                        (i, v) : [(j + 1, -v) | j < h]
                forAll (pointQry bnds) $ \i ->
                    foldPrefixF i ft `shouldBe` naive ivs l i

    prop "fromListF" $
        forAll genBounds $ \bnds@(l,h) ->
            forAll (vector (h-l+1)) $ \xs -> do
                let ft = fromListF bnds xs :: FTree (Sum Int)
                toScanl1F ft `shouldBe` scanl1 (<>) xs

    prop "binSearchF" $
        \(xs :: [NonNegative (Sum Int)], l, x) -> do
            let n = length xs
                xs' = map getNonNegative xs
                ft = fromListF (l, l + n - 1) xs'
                expected = find ((>=x) . snd) $ zip [l..] (scanl1 (<>) xs')
            classify (isJust expected) "in range" $
                binSearchF (>=x) ft `shouldBe` expected

    prop "toScanl1F" $
        \(xs :: [Sum Int]) -> do
            let n = length xs
                ft = fromListF (1, n) xs
            toScanl1F ft `shouldBe` scanl1 (<>) xs

  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    applyUpdates ivs ft = foldl' (\ft (i, x) -> mappendF i x ft) ft ivs
    applyRangeUpdates ijvs ft = foldl' (\ft (i, j, x) -> mappendRangeF i j x ft) ft ijvs

genBounds :: Gen (Int, Int)
genBounds = sized $ \n' -> do
    n <- choose (0, n')
    l <- arbitrary
    pure (l, l+n-1)

pointUpds :: (Int, Int) -> Gen [(Int, Sum Int)]
pointUpds (l,h)
    | l == h + 1 = pure []
    | otherwise  = listOf $ (,) <$> choose (l,h) <*> arbitrary

rangeUpds :: (Int, Int) -> Gen [(Int, Int, Sum Int)]
rangeUpds (l,h)
    | l == h + 1 = pure []
    | otherwise = listOf $ do
        (i, j) <- genSortedIntPair (l,h)
        v <- arbitrary
        pure (i, j, Sum v)

pointQry :: (Int, Int) -> Gen Int
pointQry (l, r) = chooseInt (l - 10, r + 10)
