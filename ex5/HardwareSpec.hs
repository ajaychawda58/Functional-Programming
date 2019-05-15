module HardwareSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Hardware (Bit (..), Carry, mapr, halfAdder, fullAdder, rippleAdder)

intToBits :: Int -> [Bit]
intToBits n
    | n < 0     = error "negative numbers not supported"
    | n == 0    = [O]
    | otherwise = reverse $ h n where h 0 = []; h i = (if (i `mod` 2) == 0 then O else I) : h (i `div` 2)

bitToInt :: Bit -> Int
bitToInt O = 0
bitToInt I = 1

bitsToInt :: [Bit] -> Int
bitsToInt = fst . (foldr (\b (r, s) -> (r + s * bitToInt b, 2*s)) (0, 1))

checkRippleAdder :: Carry -> (Int, Int) -> Expectation
checkRippleAdder c (x, y) =
    if x >= 0 && y >= 0 then
        let -- transforming input
            (xs, ys) = (intToBits x, intToBits y)
            (lx, ly) = (length xs, length ys)
            maxlen   = max lx ly
            p l as   = (replicate (maxlen - l) O) ++ as
            xs'      = p lx xs
            ys'      = p ly ys
            -- transforming output
            (rs, rc) = rippleAdder (xs', ys', c)
            result   = bitsToInt (rc:rs)
        in result `shouldBe` x + y + bitToInt c
    else return ()

spec = do
    describe "mapr" $ do
        it "mapr (\\(x, s) -> (x, (s + x) * x)) ([1, 3, 7], 5) = ([1, 3, 7], (((((5 + 7) * 7) + 3) * 3) + 1) * 1)" $
            mapr (\(x, s) -> (x, (s + x) * x)) ([1, 3, 7], 5) `shouldBe` ([1, 3, 7], (((((5 + 7) * 7) + 3) * 3) + 1) * 1)
        it "mapr (\\(x, s) -> (x, (s + x) * x)) (xs, s) = (xs, foldl (\\x s -> (s + x) * x) s)" $
            property $ \xs s -> mapr (\(x, s) -> (x, (s + x) * x)) (xs::[Int], s::Int) `shouldBe` (xs, foldr (\x s -> (s + x) * x) s xs)
        it "mapr (\\(x, s) -> (succ x, s)) (xs, s) = (map succ xs, s)" $
            property $ \xs s -> mapr (\(x, s) -> (succ x, s)) (xs::[Int], s::Int) `shouldBe` (map succ xs, s)
        it "mapr (\\(x, s) -> (succ x, 2*s)) (xs, s) = (map succ xs, s * 2^(length xs))" $
            property $ \xs s -> mapr (\(x, s) -> (succ x, 2*s)) (xs::[Int], s::Int) `shouldBe` (map succ xs, s * 2^(length xs))

    describe "halfAdder" $ do
        it "(O, O)" $ halfAdder (O, O) `shouldBe` (O, O)
        it "(O, I)" $ halfAdder (O, I) `shouldBe` (I, O)
        it "(I, O)" $ halfAdder (I, O) `shouldBe` (I, O)
        it "(I, I)" $ halfAdder (I, I) `shouldBe` (O, I)

    describe "fullAdder" $ do
        it "((O, O), O)" $ fullAdder ((O, O), O) `shouldBe` (O, O)
        it "((O, I), O)" $ fullAdder ((O, I), O) `shouldBe` (I, O)
        it "((I, O), O)" $ fullAdder ((I, O), O) `shouldBe` (I, O)
        it "((I, I), O)" $ fullAdder ((I, I), O) `shouldBe` (O, I)
        it "((O, O), I)" $ fullAdder ((O, O), I) `shouldBe` (I, O)
        it "((O, I), I)" $ fullAdder ((O, I), I) `shouldBe` (O, I)
        it "((I, O), I)" $ fullAdder ((I, O), I) `shouldBe` (O, I)
        it "((I, I), I)" $ fullAdder ((I, I), I) `shouldBe` (I, I)

    describe "rippleAdder" $ do
        it "with initial carry O" $
            property $ checkRippleAdder O
        it "with initial carry I" $
            property $ checkRippleAdder O

main = hspec spec
