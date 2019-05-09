module CalculusSpec (spec)
where

import Test.Hspec
import Calculus (Function (..), apply, derive, simplify)

size :: Function -> Integer
size (Const _) = 1
size Id = 1
size (f1 :+: f2) = size f1 + size f2
size (f1 :*: f2) = size f1 + size f2
size (f1 :^: _) = size f1 + 1
size (f1 :.: f2) = size f1 + size f2

spec = do
    describe "apply" $ do
        describe "Id :^: 2" $ do
            let f = apply (Id :^: 2)
            it "0" $ f 0 `shouldBe` 0
            it "3" $ f 3 `shouldBe` 9
            it "-3" $ f (-3) `shouldBe` 9
        describe "Id :*: Const 3 :+: Const 7" $ do
            let f = apply (Id :*: Const 3 :+: Const 7)
            it "0" $ f 0 `shouldBe` 7
            it "3" $ f 3 `shouldBe` 16
            it "-3" $ f (-3) `shouldBe` -2
        describe "(Id :^: 3) :.: (Id :*: Const 4)" $ do
            let f = apply ((Id :^: 3) :.: (Id :*: Const 4))
            it "0" $ f 0 `shouldBe` 0
            it "3" $ f 3 `shouldBe` 1728
            it "-3" $ f (-3) `shouldBe` -1728
        describe "Id :^: 0" $ do
            let f = apply (Id :^: 0)
            -- x^0 = 1 for all x != 0
            it "123" $ f 123 `shouldBe` 1
            it "-456" $ f (-456) `shouldBe` 1
        describe "Id :^: (-1)" $ do
            let f = apply (Id :^: (-1))
            -- x^-1 = 1/x for all x != 0
            it "123" $ f 123 `shouldBe` 1 / 123
            it "-456" $ f (-456) `shouldBe` - 1 / 456
        describe "Id :^: (-10)" $ do
            let f = apply (Id :^: (-10))
            -- x^-10 = 1/x^10 for all x != 0
            it "3" $ f 3 `shouldBe` 1 / (3^10)
            it "-3" $ f (-3) `shouldBe` 1 / (3^10)

    describe "apply derive" $ do
        describe "Id :^: 2" $ do
            let f = apply (derive (Id :^: 2))
            -- f x = 2 * x
            it "0" $ f 0 `shouldBe` 0
            it "3" $ f 3 `shouldBe` 6
            it "-3" $ f (-3) `shouldBe` -6
        describe "Id :*: Const 3 :+: Const 7" $ do
            let f = apply (derive (Id :*: Const 3 :+: Const 7))
            -- f x = 3
            it "0" $ f 0 `shouldBe` 3
            it "3" $ f 3 `shouldBe` 3
            it "-3" $ f (-3) `shouldBe` 3
        describe "(Id :^: 3) :.: (Id :*: Const 4)" $ do
            let f = apply (derive ((Id :^: 3) :.: (Id :*: Const 4)))
            -- f x = 192 * x^2
            it "0" $ f 0 `shouldBe` 0
            it "3" $ f 3 `shouldBe` 192*3^2
            it "-3" $ f (-3) `shouldBe` 192*(-3)^2

    describe "apply derive derive" $ do
        describe "Id" $ do
            let f = apply (derive (derive Id))
            -- f x = 0
            it "0" $ f 0 `shouldBe` 0
            it "3" $ f 3 `shouldBe` 0
            it "-3" $ f (-3) `shouldBe` 0

    describe "simplify" $ do
        describe "Id :.: Id :.: Id" $ do
            let f = simplify (Id :.: Id :.: Id)
            -- solution: Id
            it "size = 1" $ size f `shouldBe` 1
            describe "apply" $ do
                let g = apply f
                it "0" $ g 0 `shouldBe` 0
                it "3" $ g 3 `shouldBe` 3
                it "-3" $ g (-3) `shouldBe` -3
        describe "Const 1 :*: Id" $ do
            let f = simplify (Const 1 :*: Id)
            -- solution: Id
            it "size = 1" $ size f `shouldBe` 1
            describe "apply" $ do
                let g = apply f
                it "0" $ g 0 `shouldBe` 0
                it "3" $ g 3 `shouldBe` 3
                it "-3" $ g (-3) `shouldBe` -3
        describe "Const 1 :+: Const 2 :+: Id" $ do
            let f = simplify (Const 1 :+: Const 2 :+: Id)
            -- solution: Const 3 :+: Id
            it "size = 2" $ size f `shouldBe` 2
            describe "apply" $ do
                let g = apply f
                it "0" $ g 0 `shouldBe` 3
                it "3" $ g 3 `shouldBe` 6
                it "-3" $ g (-3) `shouldBe` 0
        describe "(Const 3 :*: Id) :^: 0" $ do
            let f = simplify ((Const 3 :*: Id) :^: 0)
            -- solution: Const 1
            it "size = 1" $ size f `shouldBe` 1
            describe "apply" $ do
                let g = apply f
                it "3" $ g 3 `shouldBe` 1
                it "-3" $ g (-3) `shouldBe` 1

main = hspec spec
