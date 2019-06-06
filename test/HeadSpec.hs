
module HeadSpec ( spec ) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Control.Exception (evaluate)

spec :: Spec
spec =
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23..] `shouldBe` (23 :: Int)
        prop "returns the first element of an arbitrary list" $
            \x xs -> head (x:xs) == (x :: Int)
        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException
