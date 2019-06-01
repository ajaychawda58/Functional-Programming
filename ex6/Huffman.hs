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
huffman freq = t
    where (_ :- t) = huffmanRec (huffCreateAllLeafs freq)

huffmanRec :: [With Int (Tree char)] -> With Int (Tree char)
huffmanRec ([])  = error "Need atleast one element since text contains at least two different characters."
huffmanRec (a:[])  = a
huffmanRec ((l1:-t1) : (l2:-t2) : xs) = huffmanRec $ sort (newElement : xs)  
    where newElement = (l1+l2) :-(t1:^:t2)  

huffCreateAllLeafs :: [With Int char] -> [With Int (Tree char)] 
huffCreateAllLeafs freq =  map (\(b :- e) -> (b :- Leaf e)) $ sort freq

-- b)
englishTree :: Tree Char
englishTree = huffman [8 :- 'a',1 :- 'b',3 :- 'c',4 :- 'd',13 :- 'e',2 :- 'f',2 :- 'g',6 :- 'h',7 :- 'i',1 :- 'j',1 :- 'k',4 :- 'l',2 :- 'm',7 :- 'n',8 :- 'o',2 :- 'p',1 :- 'q',6 :- 'r',6 :- 's',9 :- 't',3 :- 'u',1 :- 'v',2 :- 'w',1 :- 'x',2 :- 'y',0 :- 'z']
-- (Define a Huffman tree that contains the 26 characters from 'a' to 'z', but no capital, whitespace or punctuation)
-- Ans : Using the wikipedia data and converting frequencies of letters in text to int.

-------------------------------------------------------------------------------
-- 3) Encoding ASCII text
encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode ct text = foldl (\a x -> a ++ (bitsFromChar x codeList)) [] text 
    where codeList = codes ct 

-- helper function:
codes :: Tree char -> [(char, [Bit])]
codes a = codes' a []

codes' :: Tree char -> [Bit] -> [(char, [Bit])]
codes' (Leaf a) res = [(a, res)]
codes' (l :^: r) res =  le ++ ri
    where le = appendBit (codes' l res) O
          ri = appendBit (codes' r res) I

appendBit :: [(char, [Bit])] -> Bit -> [(char, [Bit])]
appendBit res b =  map (\(c,bs) -> (c,b:bs)) res

-- will remove once map implemented for fast look up
bitsFromChar :: Eq char => char -> [(char, [Bit])] -> [Bit]
bitsFromChar _ [] = []
bitsFromChar k ((c,bs):xs)
    | (k == c) = bs
    | otherwise = bitsFromChar k xs
    
-------------------------------------------------------------------------------
-- 4) Decoding a Huffman binary

decode :: Tree char -> [Bit] -> [char]
decode ct et = decode' ct et ct []

decode' :: Tree char -> [Bit] -> Tree char -> [char] ->[char]
decode' orig encod (Leaf a) res
    | null encod = res ++ [a] 
    | otherwise = decode' orig encod orig (res ++ [a]) 
decode' orig encod (l :^: r) res
    | f == I = decode' orig (tail encod) r res 
    | f == O = decode' orig (tail encod) l res 
    where f = head encod

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
