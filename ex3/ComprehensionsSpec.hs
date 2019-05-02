module ComprehensionsSpec (spec)
where

import Test.Hspec
import Test.QuickCheck
import Data.List
import qualified Comprehensions

f :: [(Bool, a)] -> [a]
f [] = []
f ((True, x):xs) = x:f xs
f (_:xs) = f xs

g :: [(a, b)] -> [(b, a)]
g [] = []
g ((x,y):xs) = (y,x) : g xs

h :: [(Integer, a)] -> [a]
h [] = []
h ((n, x):xs) = (rep n x) ++ h xs where rep n x = if n <= 0 then [] else x : rep (n-1) x

spec = do
    describe "f" $ do
        it "behaves like my recursive implementation" $
            property $ \xs -> Comprehensions.f (xs::[(Bool, Integer)]) == f xs

    describe "g" $ do
        it "behaves like my recursive implementation" $
            property $ \xs -> Comprehensions.g (xs::[(Integer, Bool)]) == g xs

    describe "h" $ do
        it "behaves like my recursive implementation" $
            property $ \xs -> Comprehensions.h (xs::[(Integer, Char)]) == h xs

main = hspec spec
