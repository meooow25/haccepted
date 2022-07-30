module SegTreeSpec where

import Data.Array
import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SegTree ( SegTree, adjustST, boundsST, emptyST, foldRangeST, fromListST )
import Util ( genPossiblyEmptyRange )

spec :: Spec
spec = do
    prop "multiple adjustST then foldRangeST works ok" $
        forAll genSt $ \st -> do
            let bnds = boundsST st
            forAll (pointUpds bnds) $ \ivs -> do
                let st' = adjustMany st ivs
                forAll (rangeQry bnds) $ \(i, j) ->
                    foldRangeST i j st' `shouldBe` naive ivs i j

    prop "multiple adjustST then toList works ok" $
        forAll genSt $ \st -> do
            let bnds = boundsST st
            forAll (pointUpds bnds) $ \ivs -> do
                let st' = adjustMany st ivs
                    xs = elems $ accumArray (<>) mempty bnds ivs
                toList st' `shouldBe` xs

    prop "fromListST is same as multiple adjustST" $
        forAll genSt $ \st -> do
            let (l, h) = boundsST st
                n = h - l + 1
            forAll (vector n :: Gen [Sum Int]) $ \xs -> do
                let st' = fromListST (l, h) xs
                    st'' = adjustMany st (zip [l..] xs)
                toList st' `shouldBe` toList st''

  where
    naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
    adjustMany = foldl' (\st (i, v) -> adjustST (v <>) i st)

genSt :: Gen (SegTree (Sum Int))
genSt = sized $ \n -> do
    n' <- choose (0, n)
    l <- arbitrary
    pure $ emptyST (l, l + n' - 1)

pointUpds :: (Int, Int) -> Gen [(Int, Sum Int)]
pointUpds (l, h)
    | l == h + 1 = pure []
    | otherwise = listOf $ do
        i <- choose (l, h)
        v <- arbitrary
        pure (i, Sum v)

rangeQry :: (Int, Int) -> Gen (Int, Int)
rangeQry (l, r) = genPossiblyEmptyRange (l - 10, r + 10)
