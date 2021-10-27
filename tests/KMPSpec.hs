module KMPSpec where

import Data.Array.Unboxed
import Data.List
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import KMP ( prefixFunc, prefixFuncBS )

spec :: Spec
spec = do
    prop "prefixFunc binary" $
        forAll (arbitrary :: Gen [Bool]) $ \xs -> do
            let p = prefixFunc (length xs) (xs!!)
            bounds p `shouldBe` (0, length xs - 1)
            elems p  `shouldBe` naivePrefixFunc xs
    prop "prefixFuncBS" $
        \(ASCIIString s) -> do
            let p = prefixFuncBS $ C.pack s
            bounds p `shouldBe` (0, length s - 1)
            elems p  `shouldBe` naivePrefixFunc s

naivePrefixFunc :: Eq a => [a] -> [Int]
naivePrefixFunc xs = map getCnt $ tail xsInits where
    xsInits = inits xs
    ok cur chk = chk /= cur && chk `isSuffixOf` cur
    getCnt cur = length $ last $ filter (ok cur) xsInits
