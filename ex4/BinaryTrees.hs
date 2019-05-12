module BinaryTrees
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
    deriving (Eq,Show)

left :: Tree a -> Maybe a
left Empty = Nothing
left (Node Empty a _) = Just a
left (Node x _ _) = left x

reverseTree :: Tree a -> Tree a
reverseTree Empty = Empty 
reverseTree (Node x y z) = (Node (reverseTree z) y (reverseTree x))

-- TODO Remove
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

treeTest0 = Node Empty 4 Empty
treeTest1 = Node Empty 4 (Node Empty 3 Empty)
treeTest2 = Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)) 4 (Node (Node Empty 7 Empty) 9 Empty)
