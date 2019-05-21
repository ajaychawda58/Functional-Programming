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
In case of foldm, there is no left associativity or right, it is evaluated using a balanced binary tree. The binary tree can be accessed in both ways,
either from top to bottom or from bottom to up. Whereas in foldr and foldl you can access the list only either by left or right associativity.
Example,
foldm (+) 0 [1,2,3,4] is equivalent to [(1+2)+(3+4) + 0]
It is evaluated like this,

foldm (+) 0 [1,2,3,4]                               -- foldm (+) 0 [1,2,3,4]
(+) 1 (foldm (+) 0 [2,3,4])                         -- 1 + foldm (+) 0 [2,3,4]
((+) 1 2) (foldm (+) 0 [3,4])                       -- (1+2) + foldm (+) 0 [3,4]
((+) 1 2) ((+) 3 4) (foldm (+) 0 [])                -- (1+2) + (3+4) + foldm (+) 0 []
((+) 1 2) ((+) 3 4) ((+) 0)                         -- (1+2) + (3+4) + 0

It is of the form:
foldm :: (a -> a -> a) -> a -> [a] -> a

-}


-- b) top-down
foldmTD :: (a -> a -> a) -> a -> [a] -> a
--foldmTD = undefined
foldmTD x y [] = y
foldmTD x y xs = f xs
    where   f [p] = p
            f xs = f (g xs)
            g [] = []
            g [p] = [p]
            g (m:n:xs) = x m n: g xs

-- c) bottom-up
foldmBU :: (a -> a -> a) -> a -> [a] -> a
--foldmBU = undefined
foldmBU p q = f 
  where
    f [] = q
    f (x:xs) = x `p` f (g xs)
    g (x:y:zs) = p x y : g zs
    g zs = zs
