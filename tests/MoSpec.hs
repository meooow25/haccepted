module MoSpec where

import Control.Monad.State
import Data.Array
import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Mo ( MoQuery(..), runMo, sqrtSize )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    prop "runMo" $
        -- Range sum queries.
        -- No reason to use Mo for this normally, but it's simple to use as a test.
        forAll genBndsValsQrys $ \(bnds@(l, r), xs, qrys) -> do
            let xa = listArray bnds xs
                res = evalState (runMo (sqrtSize $ r - l + 1) add rem ans qrys) 0 where
                    add = modify' . (+) . (xa!)
                    rem = modify' . subtract . (xa!)
                    ans = get
                expectedRes = map f qrys where
                    f (MoQuery ql qr qtag) = (qtag, sum $ map (xa!) [ql..qr])
            sort res `shouldBe` sort expectedRes

genBndsValsQrys :: Gen ((Int, Int), [Int], [MoQuery])
genBndsValsQrys = sized $ \n -> do
    n' <- choose (0, n)
    l <- arbitrary
    let bnds = (l, l + n' - 1)
    xs <- vector n'
    qlrs <- if n' == 0 then pure [] else listOf (genSortedIntPair bnds)
    let qrys = [MoQuery ql qr i | (i, (ql, qr)) <- zip [1..] qlrs]
    pure (bnds, xs, qrys)
