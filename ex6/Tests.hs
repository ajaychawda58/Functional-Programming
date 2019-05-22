module Main
where

import Test.Hspec
import Test.QuickCheck
import Huffman (With (..), Tree (..), Bit (..), frequencies, huffman, encode, decode)

ps :: (String -> a) -> (PrintableString -> a)
ps f = f . getPrintableString

main = hspec $ do
    describe "frequencies" $ do
        it "result contains only frequencies > 0" $
            property $ ps $ \s -> all (\(n :- _) -> n > 0) (frequencies s)
        it "result contains only entries from input" $
            property $ ps $ \s -> all (\(_ :- c) -> c `elem` s) (frequencies s)
        it "result contains all entries from input" $
            property $ ps $ \s -> let table = frequencies s in
                all (\c -> any (\(_ :- c') -> c == c') table) s
        it "result contains no duplicates (failure shows duplicate chars)" $
            property $ ps $ \s -> let chars = map (\(_ :- c) -> c) (frequencies s) in
                filter (\c -> length (filter (==c) chars) > 1) chars `shouldBe` []
        it "each result entry has the correct frequency" $
            property $ ps $ \s -> let table = frequencies s in
                sequence_ (map (\(n :- c) -> (n :- c) `shouldBe` (length (filter (==c) s) :- c)) table)

    describe "huffman" $ do
        it "hello world" $
            huffman helloWorldFrequencies `shouldBe` helloWorldTree
        it "example text (why functional programming matters)" $
            huffman whyFrequencies `shouldBe` whyTree

    describe "encode" $ do
        it "hello world" $
            encode helloWorldTree helloWorld `shouldBe` helloWorldEncoded
        it "example text (why functional programming matters)" $
            encode whyTree why `shouldBe` whyEncoded

    describe "decode" $ do
        it "hello world" $
            decode helloWorldTree helloWorldEncoded `shouldBe` helloWorld
        it "example text (why functional programming matters)" $
            decode whyTree whyEncoded `shouldBe` why

    describe "compose everything" $ do
        it "random tests: encode -> decode should be the same" $
            property $ ps $ \s ->
                if atLeastTwoDifferentChars s then
                    let ct = huffman $ frequencies s in
                        decode ct (encode ct s) `shouldBe` s
                else return ()

atLeastTwoDifferentChars :: String -> Bool
atLeastTwoDifferentChars [] = False
atLeastTwoDifferentChars (c:cs) = any (/=c) cs

helloWorld = "hello world"
helloWorldFrequencies = [1 :- ' ',1 :- 'd',1 :- 'e',1 :- 'h',3 :- 'l',2 :- 'o',1 :- 'r',1 :- 'w']
helloWorldTree = ((Leaf 'r' :^: Leaf 'w') :^: (Leaf 'e' :^: Leaf 'h')) :^: (Leaf 'l' :^: ((Leaf ' ' :^: Leaf 'd') :^: Leaf 'o'))
helloWorldEncoded = [O,I,I,O,I,O,I,O,I,O,I,I,I,I,I,O,O,O,O,I,I,I,I,O,O,O,I,O,I,I,O,I]

why =
    "As software becomes more and more complex, it\n\
    \is  more  and  more important to structure it\n\
    \well.  Well-structured  software  is  easy to\n\
    \write,   easy   to   debug,  and  provides  a\n\
    \collection  of modules that can be re-used to\n\
    \reduce future programming costs. Conventional\n\
    \languages place a conceptual limit on the way\n\
    \problems   can   be  modularised.  Functional\n\ 
    \languages  push  those  limits  back. In this\n\
    \paper we show that two features of functional\n\
    \languages    in    particular,   higher-order\n\
    \functions and lazy evaluation, can contribute\n\
    \greatly  to  modularity.  Since modularity is\n\
    \the key to successful programming, functional\n\
    \languages  are  vitally important to the real\n\
    \world."
whyFrequencies = [15 :- '\n',124 :- ' ',6 :- ',',3 :- '-',6 :- '.',1 :- 'A',1 :- 'C',1 :- 'F',1 :- 'I',1 :- 'S',1 :- 'W',49 :- 'a',7 :- 'b',24 :- 'c',16 :- 'd',54 :- 'e',10 :- 'f',15 :- 'g',11 :- 'h',32 :- 'i',2 :- 'k',33 :- 'l',19 :- 'm',34 :- 'n',45 :- 'o',13 :- 'p',37 :- 'r',32 :- 's',50 :- 't',29 :- 'u',4 :- 'v',9 :- 'w',1 :- 'x',9 :- 'y',1 :- 'z']
whyTree = ((((Leaf '\n' :^: Leaf 'g') :^: Leaf 'i') :^: (Leaf 's' :^: Leaf 'l')) :^: (((Leaf 'd' :^: ((Leaf 'v' :^: (Leaf 'k' :^: Leaf '-')) :^: Leaf 'w')) :^: Leaf 'n') :^: (Leaf 'r' :^: ((Leaf 'y' :^: Leaf 'f') :^: Leaf 'm')))) :^: (((Leaf 'o' :^: ((Leaf 'h' :^: (Leaf ',' :^: Leaf '.')) :^: Leaf 'c')) :^: (Leaf 'a' :^: Leaf 't')) :^: ((Leaf 'e' :^: ((Leaf 'p' :^: (Leaf 'b' :^: (((Leaf 'F' :^: Leaf 'I') :^: (Leaf 'A' :^: Leaf 'C')) :^: ((Leaf 'x' :^: Leaf 'z') :^: (Leaf 'S' :^: Leaf 'W'))))) :^: Leaf 'u')) :^: Leaf ' '))
whyEncoded = [I,I,O,I,O,I,I,O,I,O,O,O,I,O,I,I,I,O,O,I,O,I,O,O,O,O,I,I,I,O,I,I,O,I,I,O,I,O,O,I,I,I,O,I,O,O,I,I,O,I,I,O,O,I,I,I,I,I,O,I,O,I,O,I,I,O,O,I,O,O,I,I,I,O,O,O,O,I,I,I,I,I,I,O,O,O,O,I,O,I,I,I,O,I,I,I,I,I,O,O,O,O,I,I,O,I,I,O,O,I,I,I,I,O,I,O,O,I,O,I,O,I,O,O,O,I,I,I,O,I,I,I,I,I,O,O,O,O,I,I,O,I,I,O,O,I,I,I,I,O,O,I,I,I,O,O,O,O,I,I,I,I,I,I,O,I,O,O,O,O,I,I,I,I,O,O,I,I,O,I,O,I,I,I,O,O,I,O,O,I,O,I,O,I,I,I,O,O,O,I,I,O,I,I,O,O,O,O,O,O,O,O,I,O,O,I,O,I,I,I,I,I,I,O,I,I,I,I,I,O,O,O,O,I,I,O,I,I,O,O,I,I,I,I,I,I,I,O,I,O,O,I,O,I,O,I,O,O,O,I,I,I,I,I,I,O,I,I,I,I,I,O,O,O,O,I,I,O,I,I,O,O,I,I,I,O,O,O,I,O,I,I,I,I,I,I,O,I,O,O,I,O,O,O,O,I,I,O,I,O,I,I,I,O,I,O,O,I,O,I,I,O,I,I,I,I,I,I,O,I,I,I,O,O,O,I,I,I,O,O,I,O,I,O,I,I,O,I,I,O,I,I,O,I,I,I,O,O,I,I,I,O,I,I,I,I,O,I,I,O,I,I,O,I,I,O,O,I,I,I,O,O,O,I,I,O,I,I,O,O,O,O,O,O,I,O,O,I,I,I,I,O,O,O,O,I,I,O,O,I,I,I,O,O,I,O,I,I,I,I,I,I,I,I,I,I,O,I,O,I,I,I,I,I,I,I,O,O,O,O,I,I,O,O,I,I,O,I,O,O,I,O,I,I,O,O,I,O,I,O,I,I,O,I,I,O,I,I,O,I,I,I,O,O,I,I,I,O,I,I,I,I,O,I,I,O,I,I,O,I,I,O,O,O,I,O,O,O,I,I,I,I,I,I,O,O,I,O,I,O,O,O,O,I,I,I,O,I,I,O,I,I,O,I,O,O,I,I,I,O,I,O,O,I,I,O,I,I,O,O,I,I,I,I,I,I,O,O,O,I,O,O,I,O,I,I,I,I,I,I,I,I,O,O,I,O,I,O,O,O,I,O,O,I,I,I,O,O,I,I,I,I,O,I,I,I,O,O,O,O,O,O,O,O,O,I,O,O,I,I,O,I,I,O,O,O,O,I,I,O,I,I,I,I,O,O,I,O,O,I,O,I,O,I,I,I,I,I,I,I,I,I,I,I,O,O,I,O,I,O,O,O,I,O,O,I,I,I,O,O,I,I,I,I,I,I,I,I,I,I,O,I,I,I,O,O,O,I,I,I,I,I,I,I,I,I,O,I,O,O,O,I,I,O,O,I,I,O,I,O,I,O,I,I,O,I,I,O,O,O,O,I,I,O,O,I,O,I,O,I,I,I,I,I,I,I,O,I,O,O,I,O,I,O,I,O,O,O,I,I,I,I,I,I,I,I,O,I,O,O,O,I,I,O,I,O,O,O,O,I,O,O,I,O,O,O,O,O,I,O,I,O,O,O,I,I,O,O,O,O,I,O,I,I,I,I,I,I,I,O,I,O,O,O,O,O,O,I,O,O,I,I,I,O,O,O,O,O,I,I,O,O,I,I,I,I,O,O,I,O,O,I,I,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,I,I,I,I,I,I,I,O,O,O,O,I,I,I,O,I,I,I,I,O,I,I,I,I,I,O,O,O,O,I,O,O,O,I,I,O,I,I,O,O,I,I,I,I,O,O,O,O,I,O,I,I,I,I,O,I,I,I,O,O,I,O,O,I,O,I,O,I,O,I,I,I,I,I,I,O,O,I,I,I,O,I,O,O,I,O,I,I,I,I,I,I,O,I,O,I,O,I,I,O,O,I,I,I,O,I,I,O,I,I,O,O,O,I,O,O,I,O,I,I,I,I,O,I,I,O,O,I,O,I,I,O,O,O,I,O,O,O,I,I,I,I,O,I,I,I,O,O,O,O,O,O,O,O,O,I,I,O,I,I,O,O,O,I,O,O,O,I,I,O,I,I,I,O,O,I,I,I,I,O,O,I,I,I,O,I,I,I,O,I,I,I,O,I,I,I,O,I,I,I,I,O,I,I,O,I,I,O,I,I,O,O,I,I,I,I,I,O,I,O,O,O,I,I,O,I,O,O,O,O,O,O,O,I,O,I,I,O,I,O,I,O,O,I,I,I,I,O,I,I,I,I,O,O,O,I,O,I,O,I,O,O,O,O,I,I,I,I,I,O,O,I,I,I,O,O,O,O,O,I,O,I,O,I,I,O,O,I,O,I,O,O,I,O,I,I,I,I,I,I,I,O,I,O,I,I,O,I,I,I,O,O,O,O,I,O,I,O,I,O,O,I,O,O,I,I,O,O,O,I,O,I,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,I,O,I,O,O,O,I,I,O,O,O,O,O,O,O,I,I,I,O,I,O,O,I,O,I,O,O,O,O,I,I,I,O,I,I,I,O,I,O,O,O,O,O,I,I,I,O,O,O,O,I,O,I,I,I,I,I,O,I,O,O,O,O,I,I,I,O,I,O,I,O,O,I,I,I,I,O,O,I,I,I,I,O,I,O,I,I,I,I,O,O,I,I,I,O,O,O,O,I,O,I,I,O,O,I,I,I,I,O,O,I,I,O,I,O,O,I,O,I,I,I,I,O,I,I,I,O,I,O,O,O,I,I,I,I,I,O,O,I,I,O,O,O,I,O,I,I,I,I,O,O,O,I,I,O,I,I,I,I,I,I,O,O,O,O,I,O,I,I,I,I,I,O,I,I,I,O,O,I,O,O,I,I,O,O,I,I,I,O,I,O,O,I,I,I,O,I,O,O,I,I,I,O,O,O,O,O,O,O,I,I,O,I,O,O,O,I,I,O,I,O,O,O,I,I,O,I,O,I,O,O,O,I,I,I,I,O,O,O,I,I,I,I,O,O,I,O,I,I,I,I,I,I,I,I,I,I,O,O,I,I,I,O,I,O,O,I,O,I,I,I,I,I,I,I,I,I,I,I,I,O,I,O,I,O,I,I,O,O,I,I,I,I,I,I,O,I,I,I,I,I,O,O,O,O,I,O,O,O,I,I,O,I,I,O,O,I,I,I,O,I,O,O,I,I,O,O,O,O,I,O,O,I,O,I,I,O,O,O,I,O,O,O,I,O,O,I,O,I,I,I,I,I,I,I,I,I,I,O,I,O,I,I,O,O,O,I,I,O,I,I,O,I,O,I,I,O,O,I,I,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,I,O,I,O,O,O,I,I,O,O,O,O,O,O,O,I,I,I,O,I,O,O,I,O,I,O,O,O,O,I,I,I,O,I,I,I,O,I,O,O,O,O,O,I,I,I,O,O,O,O,I,O,I,I,I,I,I,I,I,I,O,I,O,O,I,I,O,I,I,O,O,I,O,I,O,O,I,O,O,I,I,I,I,I,I,I,O,I,I,I,O,O,I,O,O,I,O,O,O,O,O,I,O,I,I,O,O,I,I,I,I,I,I,O,O,I,I,O,O,O,I,O,I,I,I,I,O,O,O,I,I,O,I,I,O,O,I,O,I,I,I,I,I,I,I,I,O,I,O,I,O,I,O,I,O,I,O,O,I,I,O,I,O,O,I,O,I,O,I,O,O,I,O,I,I,I,I,I,I,I,O,I,O,I,I,O,O,I,O,I,O,I,I,I,I,I,O,I,I,I,O,O,I,O,O,O,O,O,I,O,O,I,O,O,O,O,O,O,I,I,O,I,O,O,I,O,I,O,I,I,O,I,O,O,I,I,O,O,O,I,I,O,I,I,I,O,I,O,O,I,I,I,I,O,O,I,I,I,O,O,I,O,I,O,O,I,O,O,I,O,O,O,O,I,O,O,I,I,I,I,I,I,O,I,I,I,O,O,I,O,O,I,O,I,O,I,O,I,I,I,I,I,I,O,I,I,O,I,O,O,I,I,I,O,O,O,I,I,I,O,I,I,I,O,I,I,I,O,O,I,O,I,O,I,O,I,I,I,I,O,I,I,O,I,I,O,I,I,O,O,O,O,I,O,I,I,I,I,O,O,O,O,I,I,I,O,I,I,I,I,O,I,I,I,O,I,I,I,O,I,I,O,I,O,I,I,O,O,I,I,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,I,O,I,O,O,O,I,I,O,O,O,O,O,O,O,I,I,I,O,I,O,O,I,O,I,O,O,O,O,I,I,I,O,I,I,I,O,I,O,O,O,O,O,I,I,I,O,O,O,O,I,O,I,I,I,I,I,I,I,I,I,I,I,I,O,O,O,I,O,I,O,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,O,I,O,O,I,O,I,O,O,I,I,O,I,O,I,I,O,O,O,I,I,O,O,I,I,I,I,O,I,I,O,O,I,I,I,O,I,O,O,I,I,O,I,O,O,I,O,I,O,I,I,I,I,I,I,I,I,I,I,O,O,I,O,O,O,O,O,I,O,O,O,O,I,I,O,O,I,O,O,I,I,O,O,O,I,I,O,O,I,O,O,I,O,I,I,I,O,O,O,O,I,I,O,O,I,O,O,O,I,I,O,O,O,I,I,O,O,O,O,O,O,O,I,I,I,O,I,I,I,O,I,I,O,I,O,I,I,O,O,I,I,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,O,O,I,O,I,I,I,I,O,I,O,O,I,O,I,O,I,O,O,O,I,I,I,O,O,I,I,I,O,I,O,I,I,O,I,O,I,I,I,O,I,O,I,I,I,O,O,I,I,I,I,I,O,O,O,I,O,O,I,O,O,I,O,I,O,O,O,I,I,I,I,O,I,I,I,O,I,O,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,I,O,O,I,O,I,O,I,I,I,I,O,O,I,I,I,O,I,O,O,I,O,I,I,I,I,I,O,O,I,I,I,O,O,O,O,I,O,I,I,O,I,I,O,I,I,O,O,O,O,I,I,I,O,I,O,I,O,I,I,O,I,I,I,O,I,I,I,I,O,O,O,O,O,O,O,O,O,O,O,I,O,I,I,O,I,I,O,O,I,O,I,O,I,O,I,I,O,O,I,I,O,I,I,I,O,O,I,I,I,I,I,I,I,O,I,I,I,O,O,O,I,I,I,I,I,I,O,I,I,I,I,I,O,O,O,O,I,O,O,O,I,I,O,I,I,O,O,I,I,I,O,I,O,O,I,I,O,O,O,O,I,I,O,I,I,O,I,I,I,O,O,I,O,O,I,O,I,I,I,I,I,I,I,I,I,I,O,I,O,I,I,I,I,O,O,O,O,I,O,I,O,I,I,O,O,I,I,I,I,O,O,I,I,I,O,I,I,I,I,I,O,O,O,O,I,O,O,O,I,I,O,I,I,O,O,I,I,I,O,I,O,O,I,I,O,O,O,O,I,I,O,I,I,O,I,I,I,O,O,I,I,I,O,O,O,I,O,O,I,O,O,O,O,O,O,I,O,I,I,I,O,O,I,O,O,I,I,O,O,I,I,I,O,I,O,O,I,O,I,O,I,I,O,O,O,I,I,I,O,O,I,I,I,I,O,I,I,I,O,O,O,I,I,I,O,O,I,O,I,I,O,I,I,I,O,O,I,I,I,O,O,I,I,I,I,O,O,O,O,I,O,O,O,I,O,O,I,I,I,O,I,I,I,O,I,I,O,O,I,I,I,I,I,I,I,O,I,O,O,O,I,I,O,I,O,O,O,O,O,O,O,I,O,I,I,O,I,O,I,O,O,I,I,I,I,O,I,I,I,I,O,O,O,I,O,I,O,I,O,O,O,O,I,I,O,O,I,O,I,O,I,I,I,O,I,I,I,O,I,I,I,O,I,I,O,I,O,I,I,O,O,I,I,I,O,I,I,O,O,O,I,I,O,O,O,O,I,O,I,I,O,I,O,O,O,I,I,O,O,O,O,O,O,O,I,I,I,O,I,O,O,I,O,I,O,O,O,O,I,I,I,O,I,I,I,O,I,O,O,O,O,O,I,I,I,O,O,O,O,I,O,I,I,I,I,I,I,I,O,I,O,O,I,I,O,I,I,O,O,I,I,I,I,I,I,O,I,O,O,I,O,O,O,O,O,I,I,O,I,I,I,O,I,O,O,O,I,I,O,O,I,I,O,I,I,I,O,O,I,I,I,O,O,O,I,O,I,I,I,I,I,I,O,I,O,O,I,O,O,O,O,I,I,O,I,O,I,I,I,O,I,O,O,I,O,I,I,O,I,I,I,I,I,I,O,I,I,I,O,O,O,I,I,I,I,O,I,I,I,O,O,I,O,O,I,I,O,O,I,I,I,O,I,I,O,I,I,O,O,I,O,I,O,O,O,I,I,O,O,O,O,O,O,I,O,O,I,I,I,O,O,O,O,I,I,O,O,O,I,I,O,I,O,O,O,I,O,O,I,O,I,I]
