{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module SuffixArraySpec where

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.Array.Unboxed
import Data.Maybe
import Data.Ord
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IM

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SuffixArray
import Misc ( unique )
import Sort
import Util ( genBinaryBS, genASCIIBS )

spec :: Spec
spec = do
    prop "build binary" $
        testBuild genBinaryBS
    prop "build ASCII" $
        testBuild genASCIIBS
  where
    testBuild gen =
        forAll gen $ \s -> do
            let sa = buildSufArray (C.length s) (fromEnum . C.index s)
                checkSufArray sa = isSorted (<=) $ map ((`C.drop` s) . id) $ elems sa
            sa `shouldSatisfy` checkSufArray

isSorted :: Ord a => (a -> a -> Bool) -> [a] -> Bool
isSorted leq xs = and $ zipWith leq xs (tail xs)
