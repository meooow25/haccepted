{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
module SegTreeLazySpec where

import Data.Array
import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SegTreeLazy
    ( LazySegTree
    , Action(..)
    , adjustLST
    , boundsLST
    , foldRangeLST
    , fromListLST
    , toListLST
    , updateRangeLST
    )
import SegTreeSpec ( pointUpds, rangeQry )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    prop "multiple adjustLST then foldRangeLST works ok" $
        forAll genSt $ \st -> do
            let bnds = boundsLST st
            forAll (pointUpds bnds) $ \ivs -> do
                let st' = adjustMany st ivs
                forAll (rangeQry bnds) $ \(i, j) ->
                    fst (foldRangeLST i j st') `shouldBe` naive ivs i j

    prop "multiple adjustLST then toListLST works ok" $
        forAll genSt $ \st -> do
            let bnds = boundsLST st
            forAll (pointUpds bnds) $ \ivs -> do
                let st' = adjustMany st ivs
                    xs = elems $ accumArray (<>) mempty bnds ivs
                map fst (toListLST st') `shouldBe` xs

    prop "multiple updateRangeLST then foldRangeLST works ok" $
        forAll genSt $ \st -> do
            let (l, r) = boundsLST st
            forAll (rangeUpds (l, r)) $ \ijvs -> do
                let st' = applyRangeUpdates st ijvs
                    ivs = do
                        (i, j, v) <- ijvs
                        map (,v) [i..j]
                forAll (rangeQry (l, r)) $ \(i, j) ->
                    fst (foldRangeLST i j st') `shouldBe` naive ivs i j

    prop "fromListLST is same as multiple adjustLST" $
        forAll genSt $ \st -> do
            let (l, r) = boundsLST st
                n = r - l + 1
            forAll (vector n :: Gen [Sum Int]) $ \xs -> do
                let st' = fromListLST (l, r) $ map (,1) xs :: LazySegTree (Sum Int) SumLen
                    st'' = adjustMany st (zip [l..] xs)
                toListLST st' `shouldBe` toListLST st''

  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    adjustMany st ivs = foldl' (\st (i, v) -> adjustLST (\(x, _) -> (x <> v, 1)) i st) st ivs
    applyRangeUpdates st ijvs = foldl' (\st (i, j, v) -> updateRangeLST v i j st) st ijvs


-- Can add a value to all elements in a range
type RangeAddSegTree = LazySegTree (Sum Int) SumLen
type SumLen = (Sum Int, Sum Int)

instance Action (Sum Int) SumLen where
    act (s, l) u = (s + u * l, l)

genSt :: Gen RangeAddSegTree
genSt = sized $ \n -> do
    n' <- choose (0, n)
    l <- arbitrary
    pure $ fromListLST (l, l + n' - 1) $ repeat (0, 1)

rangeUpds :: (Int, Int) -> Gen [(Int, Int, Sum Int)]
rangeUpds (l, h)
    | l == h + 1 = pure []
    | otherwise = listOf $ do
        (i, j) <- genSortedIntPair (l, h)
        v <- arbitrary
        pure (i, j, v)
