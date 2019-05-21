module MapReduce
where

-- a)
{-

Please write your answer in this multiline comment.

-}


-- b) top-down
foldmTD :: (a -> a -> a) -> a -> [a] -> a
--foldmTD = undefined
foldmTD f e [] = return e
foldmTD f e (x: xs) = do {y <- f e x; foldm f y xs}


-- c) bottom-up
foldmBU :: (a -> a -> a) -> a -> [a] -> a
foldmBU = undefined
