module CharSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Data.Char
import qualified Char

transform :: String -> [Bool] -> String
transform "" _ = ""
transform s [] = s
transform (c:cs) (t:ts) = ((if t then toUpper else toLower) c) : (transform cs ts) where

spec = do
    describe "equal" $ do
        it "\"\" \"\" => True" $
            Char.equal "" "" `shouldBe` True
        it "Ralf raLF => True" $
            Char.equal "Ralf" "raLF" `shouldBe` True
        it "Ralf Sebastian => False" $
            Char.equal "Ralf" "Sebastian" `shouldBe` False
        it "Sebastian Seba5tian => False" $
            Char.equal "Sebastian" "Seba5tian" `shouldBe` False
        it "random positive tests" $
            property $ \s t1 t2 -> Char.equal (transform s t1) (transform s t2)
        it "random negative tests" $
            property $ \s -> not $ Char.equal s ('x' : s)

    describe "isNumeral" $ do
        it "\"\" => True" $
            Char.isNumeral "" `shouldBe` True
        it "0123456789 => True" $
            Char.isNumeral "0123456789" `shouldBe` True
        it "1a => False" $
            Char.isNumeral "1a" `shouldBe` False
        it "b2 => False" $
            Char.isNumeral "b2" `shouldBe` False
        it "0123456789 => True" $
            Char.isNumeral "0123456789" `shouldBe` True
        it "random positive tests" $
            property $ \x -> Char.isNumeral $ show $ abs (x :: Integer)
        it "random negative tests" $
            property $ \x -> not $ Char.isNumeral $ x ++ "c"

    describe "isBlank" $ do
        it "\"\" => True" $
            Char.isBlank "" `shouldBe` True
        it "\" \" => True" $
            Char.isBlank " " `shouldBe` True
        it "a => False" $
            Char.isBlank "a" `shouldBe` False
        it "random positive tests" $
            property $ \x -> Char.isBlank $ replicate x ' '
        it "random negative tests" $
            property $ \x -> not $ Char.isBlank $ x ++ "z"

    describe "shift" $ do
        it " 3 A => D" $
            Char.shift 3 'A' `shouldBe` 'D'
        it "-3 D => A" $
            Char.shift (-3) 'D' `shouldBe` 'A'
        it " 5 K => P" $
            Char.shift 5 'K' `shouldBe` 'P'
        it "-5 P => K" $
            Char.shift (-5) 'P' `shouldBe` 'K'
        it " 7 W => D" $
            Char.shift 7 'W' `shouldBe` 'D'
        it "-7 D => W" $
            Char.shift (-7) 'D' `shouldBe` 'W'
        it "random shift 26" $
            property $ \c -> if c >= 'A' && c <= 'Z' then Char.shift 26 c == c else True
        it "random shift forwards and backwards" $
            property $ \c n -> if c >= 'A' && c <= 'Z' then Char.shift (-n) (Char.shift n c) == c else True
        it "random shift sum" $
            property $ \c n1 n2 -> if c >= 'A' && c <= 'Z' then Char.shift n2 (Char.shift n1 c) == Char.shift (n1 + n2) c else True

main = hspec spec
