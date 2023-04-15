{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
module SegTreeLazySpec where

import Data.Array
import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SegTreeLazy
    ( LazySegTree
    , adjustLST
    , foldRangeLST
    , fromListLST
    , foldrLST
    , updateRangeLST
    )
import SegTreeSpec ( genBounds, pointUpds, rangeQry )
import Misc ( Action(..) )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    prop "many adjustLST then foldRangeLST" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                let st = adjustMany (emptyST bnds) ivs
                forAll (rangeQry bnds) $ \(i, j) ->
                    fst (foldRangeLST i j st) `shouldBe` naive ivs i j

    prop "many adjustLST then foldrLST" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                let st = adjustMany (emptyST bnds) ivs
                    xs = elems $ accumArray (<>) mempty bnds ivs
                foldrLST ((:) . fst) [] st `shouldBe` xs

    prop "many updateRangeLST then foldRangeLST" $
        forAll genBounds $ \bnds ->
            forAll (rangeUpds bnds) $ \ijvs -> do
                let st = applyRangeUpdates (emptyST bnds) ijvs
                    ivs = do
                        (i, j, v) <- ijvs
                        map (,v) [i..j]
                forAll (rangeQry bnds) $ \(i,j) ->
                    fst (foldRangeLST i j st) `shouldBe` naive ivs i j

    prop "fromListLST is same as many adjustLST" $
        forAll genBounds $ \bnds@(l,h) ->
            forAll (vector (h-l+1)) $ \xs -> do
                let st = fromListLST bnds $ map (,1) xs :: RangeAddSegTree
                    st' = adjustMany (emptyST bnds) (zip [l..] xs)
                foldrLST (:) [] st `shouldBe` foldrLST (:) [] st'

  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    adjustMany = foldl' (\st (i, v) -> adjustLST (\(x, _) -> (x <> v, 1)) i st)
    applyRangeUpdates = foldl' (\st (i, j, v) -> updateRangeLST v i j st)


-- Can add a value to all elements in a range
type RangeAddSegTree = LazySegTree (Sum Int) SumLen
type SumLen = (Sum Int, Sum Int)

instance Action (Sum Int) SumLen where
    act (s, l) u = (s + u * l, l)

emptyST :: (Int, Int) -> RangeAddSegTree
emptyST bnds = fromListLST bnds $ repeat (0, 1)

rangeUpds :: (Int, Int) -> Gen [(Int, Int, Sum Int)]
rangeUpds (l, h)
    | l == h + 1 = pure []
    | otherwise = listOf $ do
        (i, j) <- genSortedIntPair (l, h)
        v <- arbitrary
        pure (i, j, v)
