module Main
where

import Test.Hspec
import qualified ListsSpec
import qualified ComprehensionsSpec
import qualified HigherOrderSpec

spec = do
    describe "Lists" ListsSpec.spec
    describe "Comprehensions" ComprehensionsSpec.spec
    describe "HigherOrder" HigherOrderSpec.spec

main = hspec spec
