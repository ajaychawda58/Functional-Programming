module Main
where

import Test.Hspec
import qualified BinaryTreesSpec
import qualified CalculusSpec
import qualified FoldSpec

spec = do
    describe "BinaryTrees" BinaryTreesSpec.spec
    describe "Calculus" CalculusSpec.spec
    describe "Fold" FoldSpec.spec

main = hspec spec
