module Fold
where

allTrue :: [Bool] -> Bool
allTrue = foldl (&&) True
-- allTrue = foldr (&&) True

allFalse :: [Bool] -> Bool
allFalse = foldl (\accumulative element -> accumulative && not element) True
-- allFalse = foldr (\x y-> x && not y) True

member :: (Eq a) => a -> [a] -> Bool
member k = foldl (\ac el-> if ((not ac) && k==el) then True else ac) False 

smallest :: [Int] -> Int
smallest = foldl (\ac el-> if ac > el then el else ac) (maxBound :: Int) 

largest :: [Int] -> Int
largest = foldl (\ac el-> if ac < el then el else ac) (minBound :: Int) 

-- If both recursion schemes are applicable, which one is preferable in terms of running time?
-- foldl' is tail recursive so foldl' is more space and time efficient than foldr.
