module ArbitraryInstances where

import Test.QuickCheck

import MInt ( MInt(..), mm )

instance Arbitrary MInt where
    arbitrary = MInt <$> choose (0, mm-1)
