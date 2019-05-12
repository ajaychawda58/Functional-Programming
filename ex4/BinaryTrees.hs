module BinaryTrees
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
    deriving (Eq,Show)

left :: Tree a -> Maybe a
--left = undefined
left (Node EmptyTree EmptyTree EmptyTree) = Nothing
left (Node EmptyTree a _) = Just a
left (Node x _ _) = left x


reverseTree :: Tree a -> Tree a
reverseTree = undefined
