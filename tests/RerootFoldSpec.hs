module RerootFoldSpec where

import Data.Array
import Data.Graph
import Data.List
import Data.Ord
import Data.Tree

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import RerootFold ( foldReroot )
import Util ( genTreeG )

spec :: Spec
spec = do
    prop "foldReroot" $
        forAll genTreeG $ \g ->
            forAll (choose (bounds g)) $ \root -> do
                let [t] = dfs g [root]
                    tt = fmap (\u -> let [t'] = dfs g [u] in (u, sortTree t')) t
                foldReroot (\u ts -> sortTree (Node u ts)) (flip (:)) [] t `shouldBe` tt

sortTree :: Ord a => Tree a -> Tree a
sortTree (Node u ts) = Node u $ map sortTree $ sortBy (comparing rootLabel) ts
