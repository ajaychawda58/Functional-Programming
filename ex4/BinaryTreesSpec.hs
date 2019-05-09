module BinaryTreesSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import BinaryTrees (Tree (..), left, reverseTree)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = oneof [return Empty, liftM3 Node arbitrary arbitrary arbitrary]

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

spec = do
    describe "left" $ do
        it "correct result for empty tree" $
            left (Empty::Tree Int) `shouldBe` Nothing
        it "correct result for non-empty tree" $
            property $ \l x r -> left (Node l (x::Int) r) == Just (fromMaybe x (left l))

    describe "reverseTree" $ do
        it "compliant with specification" $
            property $ \t -> inorder (reverseTree (t::Tree Int)) == reverse (inorder t)

main = hspec spec
