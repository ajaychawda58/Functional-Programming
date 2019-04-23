module Main
where

import Test.Hspec
import qualified CharSpec
import qualified PoemSpec

spec = do
    describe "Char" CharSpec.spec
    describe "Poem" PoemSpec.spec

main = hspec spec
