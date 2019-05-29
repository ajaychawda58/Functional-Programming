module Huffman
where
-- feel free to import some useful packages
import Data.List
-------------------------------------------------------------------------------
-- Provided data types

infix 1 :-
data With a b = a :- b
    deriving (Show)

instance (Eq a) => Eq (With a b) where
    (a :- _) == (b :- _)  =  a == b
instance (Ord a) => Ord (With a b) where
    (a :- _) <= (b :- _)  =  a <= b

data Tree elem = Leaf elem | Tree elem :^: Tree elem
    deriving (Show, Eq, Ord)

data Bit = O | I
    deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- 1) Constructing a frequency table

frequencies :: (Ord char) => [char] -> [With Int char]
frequencies text = [(length e :- head e)|e <-gt]
    where gt = group $ sort text 


-------------------------------------------------------------------------------
-- 2) Constructing a Huffman tree

-- a)
huffman :: [With Int char] -> Tree char
huffman = undefined

-- b)
englishTree :: Tree Char
englishTree = undefined
-- (Define a Huffman tree that contains the 26 characters from 'a' to 'z', but no capital, whitespace or punctuation)


-------------------------------------------------------------------------------
-- 3) Encoding ASCII text

encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode = undefined

-- helper function:
codes :: Tree char -> [(char, [Bit])]
codes = undefined


-------------------------------------------------------------------------------
-- 4) Decoding a Huffman binary

decode :: Tree char -> [Bit] -> [char]
decode = undefined


-------------------------------------------------------------------------------
-- Some test data.

why :: String
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
