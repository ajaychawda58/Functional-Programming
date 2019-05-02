module ListsSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Data.List
import qualified Lists

spec = do
    describe "prod" $ do
        it "behaves like Prelude.product" $
            property $ \xs -> Lists.prod xs == Prelude.product xs

    describe "contains" $ do
        it "behaves like Prelude.elem" $
            property $ \x xs -> Lists.contains x xs == Prelude.elem x xs

    describe "nth" $ do
        it "for valid indexes it behaves like (!!)" $
            property $ \n xs -> n < 0 || (fromInteger n) >= length (xs::[Integer]) || Lists.nth n xs == Just (xs!!(fromInteger n))
        it "for negative indexes it returns Nothing" $
            property $ \n xs -> n >= 0 || Lists.nth n (xs::[Integer]) == Nothing
        it "for too large indexes it returns Nothing" $
            property $ \n xs -> (fromInteger n) < length xs || Lists.nth n (xs::[Integer]) == Nothing

    describe "remove" $ do
        it "behaves like filter (/=x)" $
            property $ \x xs -> Lists.remove x xs == filter (/=x) xs

    describe "suffixes" $ do
        it "behaves like Data.List.tails" $
            property $ \xs -> Lists.suffixes (xs::[Integer]) == tails xs

main = hspec spec
