module SegTreeSpec where

import Data.Array
import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SegTree ( adjustST, emptyST, foldRangeST, fromListST )
import Util ( genPossiblyEmptyRange )

spec :: Spec
spec = do
    prop "many adjustST then foldRangeST" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                let st = adjustMany (emptyST bnds) ivs
                forAll (rangeQry bnds) $ \(i, j) ->
                    foldRangeST i j st `shouldBe` naive ivs i j

    prop "many adjustST then foldrST" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                let st = adjustMany (emptyST bnds) ivs
                    xs = elems $ accumArray (<>) mempty bnds ivs
                toList st `shouldBe` xs

    prop "fromListST is same as many adjustST" $
        forAll genBounds $ \bnds@(l,h) ->
            forAll (vector (h-l+1)) $ \xs -> do
                let st = fromListST bnds xs
                    st' = adjustMany (emptyST bnds) (zip [l..] xs)
                toList st `shouldBe` toList st'

  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    adjustMany = foldl' (\st (i, v) -> adjustST (v <>) i st)

genBounds :: Gen (Int, Int)
genBounds = sized $ \n' -> do
    n <- choose (0, n')
    l <- arbitrary
    pure (l, l+n-1)

pointUpds :: (Int, Int) -> Gen [(Int, Sum Int)]
pointUpds (l, h)
    | l == h + 1 = pure []
    | otherwise  = listOf $ (,) <$> choose (l,h) <*> arbitrary

rangeQry :: (Int, Int) -> Gen (Int, Int)
rangeQry (l, r) = genPossiblyEmptyRange (l - 10, r + 10)
