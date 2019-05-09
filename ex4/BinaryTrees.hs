module BinaryTrees
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
    deriving (Eq,Show)

left :: Tree a -> Maybe a
left = undefined

reverseTree :: Tree a -> Tree a
reverseTree = undefined
