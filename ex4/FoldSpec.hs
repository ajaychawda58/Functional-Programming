module FoldSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Fold (allTrue, allFalse, member, smallest, largest)

spec = do
    describe "allTrue" $ do
        it "behaves like (all id)" $
            property $ \xs -> allTrue xs == all id xs

    describe "allFalse" $ do
        it "behaves like (not . any id)" $
            property $ \xs -> allFalse xs == not (any id xs)

    describe "member" $ do
        it "behaves like elem" $
            property $ \x xs -> member (x::Integer) xs == elem x xs

    describe "smallest" $ do
        it "result is contained in list" $
            property $ \x xs -> elem (smallest (x:xs)) (x:xs)
        it "no smaller element in list" $
            property $ \x xs -> all (>= smallest (x:xs)) (x:xs)

    describe "largest" $ do
        it "result is contained in list" $
            property $ \x xs -> elem (largest (x:xs)) (x:xs)
        it "no larger element in list" $
            property $ \x xs -> all (<= largest (x:xs)) (x:xs)

main = hspec spec
