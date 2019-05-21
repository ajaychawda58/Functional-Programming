module MapReduce
where

-- a)
{-

Please write your answer in this multiline comment.
foldl:
foldl is left associative and is tail recursive (iterates through the list first and then returns value afterwards)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

Since thhe left fold, foldl , associates to the left. That is:
foldl (+) 0 [1, 2, 3]       -- is equivalent to ((0 + 1) + 2) + 3

foldr:
foldr is right associative but here each iteration applies f to the next value and the result of folding the rest of the list.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs

Example,
foldr (+) 0 [1, 2, 3]       -- is equivalent to 1 + (2 + (3 + 0))
It is evaluated like this,

foldr (+) 0 [1, 2, 3]                   --foldr (+) 0 [1,2,3]
(+) 1 (foldr (+) 0 [2, 3])              -- 1 + foldr (+) 0 [2,3]
(+) 1 ((+) 2 (foldr (+) 0 [3]))         -- 1 + (2 + foldr (+) 0 [3])
(+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))  -- 1 + (2 + (3 + foldr (+) 0 []))
(+) 1 ((+) 2 ((+) 3 0))                 -- 1 + (2 + (3 + 0 ))

foldm:
In case of foldm, there is no left associativity or right, it is evaluated using a balanced binary tree.
Example,
foldm (+) 0 [1,2,3,4] is equivalent to [(1+2)+(3+4) + 0]
It is evaluated like this,

TODO

It is of the form:
foldm :: (a -> a -> a) -> a -> [a] -> a

-}


-- b) top-down
foldmTD :: (a -> a -> a) -> a -> [a] -> a
--foldmTD = undefined
--foldmTD f e Empty =  e
--foldmTD f e x = do {y <- f e x; foldmTD f y x}


-- c) bottom-up
foldmBU :: (a -> a -> a) -> a -> [a] -> a
foldmBU = undefined
