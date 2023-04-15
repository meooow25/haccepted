module FenwickMutSpec where

import Data.Array.IO
import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import FenwickMut ( FenwickMut, emptyFM, foldPrefixFM, foldRangeFM, mappendFM, mappendRangeFM )
import FenwickSpec ( genBounds, pointQry, pointUpds, rangeUpds )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    prop "updates, queries" $
        forAll genBounds $ \bnds@(l,_) ->
            forAll (pointUpds bnds) $ \ivs ->
                forAll (pointQry bnds) $ \j -> do
                    ft <- emptyFM bnds :: IO (FenwickMut IOArray (Sum Int))
                    applyUpdates ft ivs
                    res <- foldPrefixFM ft j
                    res `shouldBe` naive ivs l j

    prop "updates, range queries" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs ->
                forAll (genSortedIntPair bnds) $ \(i, j) -> do
                    ft <- emptyFM bnds :: IO (FenwickMut IOArray (Sum Int))
                    applyUpdates ft ivs
                    res <- foldRangeFM ft i j
                    res `shouldBe` naive ivs i j

    prop "range updates, queries" $
        forAll genBounds $ \bnds@(l,h) ->
            forAll (rangeUpds bnds) $ \ijvs ->
                forAll (pointQry bnds) $ \i -> do
                    ft <- emptyFM bnds :: IO (FenwickMut IOArray (Sum Int))
                    applyRangeUpdates ft ijvs
                    let ivs = do
                            (i, j, v) <- ijvs
                            (i, v) : [(j + 1, -v) | j < h]
                    res <- foldPrefixFM ft i
                    res `shouldBe` naive ivs l i
  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    applyUpdates ft = traverse_ $ uncurry (mappendFM ft)
    applyRangeUpdates ft = traverse_ $ \(i, j, x) -> mappendRangeFM ft i j x
