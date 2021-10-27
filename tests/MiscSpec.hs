{-# LANGUAGE ScopedTypeVariables #-}
module MiscSpec where

import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck

import Misc ( foldExclusive )

spec :: Spec
spec = do
    prop "foldExclusive" $
        \(xs :: [Int]) -> foldExclusive (+) 0 xs `shouldBe` naiveFoldExclusive (+) 0 xs

naiveFoldExclusive :: (b -> a -> b) -> b -> [a] -> [b]
naiveFoldExclusive f b as = map (foldl' f b) $ zipWith (++) (init $ inits as) (tail $ tails as)
