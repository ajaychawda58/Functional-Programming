module MapReduceSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import MapReduce (foldmTD, foldmBU)

spec = do
    describe "foldmTD" $ do
        it "foldmTD (+) 0 = foldr (+) 0" $
            property $ \xs -> foldmTD (+) (0::Int) xs `shouldBe` foldr (+) 0 xs
        it "foldmTD (*) 1 = foldr (*) 1" $
            property $ \xs -> foldmTD (*) (1::Int) xs `shouldBe` foldr (*) 1 xs

    describe "foldmBU" $ do
        it "foldmBU (+) 0 = foldr (+) 0" $
            property $ \xs -> foldmBU (+) (0::Int) xs `shouldBe` foldr (+) 0 xs
        it "foldmBU (*) 1 = foldr (*) 1" $
            property $ \xs -> foldmBU (*) (1::Int) xs `shouldBe` foldr (*) 1 xs

main = hspec spec
