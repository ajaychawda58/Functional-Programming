module MapReduce
where

-- a)
{-

Please write your answer in this multiline comment.

-}


-- b) top-down
foldmTD :: (a -> a -> a) -> a -> [a] -> a
--foldmTD = undefined
--foldmTD f e Empty =  e
--foldmTD f e x = do {y <- f e x; foldmTD f y x}


-- c) bottom-up
foldmBU :: (a -> a -> a) -> a -> [a] -> a
foldmBU = undefined
