module Main
where

import Test.Hspec
import qualified HardwareSpec
import qualified MapReduceSpec

spec = do
    describe "Hardware" HardwareSpec.spec
    describe "MapReduce" MapReduceSpec.spec

main = hspec spec
