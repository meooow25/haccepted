module SegTreeMutSpec where

import Data.Array
import Data.Array.IO
import Data.Foldable
import Data.Maybe
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SegTreeMut
    ( SegTreeMut
    , adjustSTM
    , binSearchSTM
    , emptySTM
    , fromListSTM
    , foldRangeSTM
    , foldrSTM
    )
import SegTreeSpec ( genBounds, pointUpds, rangeQry )
import Util ( genPossiblyEmptyRange )

spec :: Spec
spec = do
    prop "many adjustSTM then foldRangeSTM" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs ->
                forAll (rangeQry bnds) $ \(i,j) -> do
                    st <- emptySTM bnds :: IO SumSegTree
                    adjustMany st ivs
                    res <- foldRangeSTM st i j
                    res `shouldBe` naive ivs i j

    prop "many adjustSTM then foldrSTM" $
        forAll genBounds $ \bnds ->
            forAll (pointUpds bnds) $ \ivs -> do
                st <- emptySTM bnds :: IO SumSegTree
                adjustMany st ivs
                res <- foldrSTM st (:) []
                let xs = elems $ accumArray (<>) mempty bnds ivs
                res `shouldBe` xs

    prop "fromListSTM is same as many adjustSTM" $
        forAll genBounds $ \bnds@(l,r) -> do
            let n = r - l + 1
            forAll (vector n) $ \xs -> do
                st1 <- fromListSTM bnds xs :: IO SumSegTree
                ys1 <- foldrSTM st1 (:) []
                st2 <- emptySTM bnds :: IO SumSegTree
                adjustMany st2 (zip [l..] xs)
                ys2 <- foldrSTM st2 (:) []
                ys1 `shouldBe` ys2

    prop "many adjustSTM then binSearchSTM" $
        forAll genBounds $ \bnds@(l,r) ->
            forAll (pointUpds bnds) $ \ivs' -> do
                let ivs = [(i,abs v) | (i,v) <- ivs']
                forAll (binSearchQry bnds) $ \((qi,qj),qx) -> do
                    let xs = [(j, naive ivs qi j) | j <- [max l qi .. min r qj]]
                        expected = find ((>=qx) . snd) xs
                    classify (isJust expected) "in range" $ do
                        st <- emptySTM bnds :: IO SumSegTree
                        adjustMany st ivs
                        res <- binSearchSTM st qi qj (>=qx)
                        res `shouldBe` expected
  where
    naive ivs i j = fold [v | (k,v) <- ivs, i <= k && k <= j]
    adjustMany st = traverse_ (\(i,v) -> adjustSTM st i (v <>))

type SumSegTree = SegTreeMut IOArray (Sum Int)

binSearchQry :: (Int, Int) -> Gen ((Int, Int), Sum Int)
binSearchQry (l,r) = (,) <$> genPossiblyEmptyRange (l-10, r+10) <*> arbitrary
