module BinaryTrees
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
    deriving (Eq,Show)

left :: Tree a -> Maybe a
--left = undefined
left (Node Empty a Empty) = Nothing
left (Node x a Empty) = maybe x
left (Node x _ _) = left x


reverseTree :: Tree a -> Tree a
reverseTree = undefined