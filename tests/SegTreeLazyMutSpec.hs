{-# LANGUAGE FlexibleContexts, TupleSections #-}
module SegTreeLazyMutSpec where

import Data.Array
import Data.Array.IO
import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SegTreeLazyMut
    ( LazySegTreeMut
    , adjustLSTM
    , foldRangeLSTM
    , fromListLSTM
    , foldrLSTM
    , updateRangeLSTM
    )
import SegTreeSpec ( genBounds, pointUpds, rangeQry )
import SegTreeLazySpec ( SumLen, rangeUpds )

spec :: Spec
spec = do
    prop "many adjustLSTM then foldRangeLSTM" $
        forAll genBounds $ \bnds ->
            forAll ((,) <$> pointUpds bnds <*> rangeQry bnds) $ \(ivs, (i,j)) -> do
                st <- emptyST bnds
                adjustMany st ivs
                (res, _) <- foldRangeLSTM st i j
                res `shouldBe` naive ivs i j

    prop "many adjustLSTM then foldrLSTM" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                st <- emptyST bnds
                adjustMany st ivs
                res <- foldrLSTM st ((:) . fst) []
                let xs = elems $ accumArray (<>) mempty bnds ivs
                res `shouldBe` xs

    prop "many updateRangeLSTM then foldRangeLSTM" $
        forAll genBounds $ \bnds -> do
            forAll ((,) <$> rangeUpds bnds <*> rangeQry bnds) $ \(ijvs, (i,j)) -> do
                st <- emptyST bnds
                applyRangeUpdates st ijvs
                let ivs = [(k,v) | (i,j,v) <- ijvs, k <- [i..j]]
                (res, _) <- foldRangeLSTM st i j
                res `shouldBe` naive ivs i j

    prop "fromListLSTM is same as many adjustLSTM" $
        forAll genBounds $ \bnds@(l,r) -> do
            let n = r - l + 1
            forAll (vector n :: Gen [Sum Int]) $ \xs -> do
                st1 <- fromListLSTM bnds $ map (,1) xs :: IO RangeAddSegTree
                ys1 <- foldrLSTM st1 ((:) . fst) []
                st2 <- emptyST bnds
                adjustMany st2 (zip [l..] xs)
                ys2 <- foldrLSTM st2 ((:) . fst) []
                ys1 `shouldBe` ys2

  where
    naive ivs i j = fold [v | (k,v) <- ivs, i <= k && k <= j]
    adjustMany st = traverse_ (\(i,v) -> adjustLSTM st i (\(x, _) -> (x <> v, 1)))
    applyRangeUpdates st = traverse_ (\(i,j,v) -> updateRangeLSTM st i j v)

-- Can add a value to all elements in a range
type RangeAddSegTree = LazySegTreeMut IOArray IOArray (Sum Int) SumLen

emptyST :: (Int, Int) -> IO RangeAddSegTree
emptyST bnds = fromListLSTM bnds $ repeat (0,1)
