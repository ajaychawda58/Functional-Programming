module Fold
where

allTrue :: [Bool] -> Bool
allTrue = foldl (&&) True
-- allTrue = foldr (&&) True
-- allTrue xs = foldl (\acc x -> if x==True then True else False) True xs   --alternate

allFalse :: [Bool] -> Bool
allFalse = foldl (\accumulative element -> accumulative && not element) True
-- allFalse = foldr (\x y-> x && not y) True
-- allFalse xs = foldl (\acc x -> if x==False then True else False) False xs    --alternate

member :: (Eq a) => a -> [a] -> Bool
member k = foldl (\ac el-> if ((not ac) && k==el) then True else ac) False 
--member y ys = foldl (\acc x -> if x==y then True else acc) False ys       --alternate

smallest :: [Int] -> Int
smallest = foldl (\ac el-> if ac > el then el else ac) (maxBound :: Int) 
--smallest = foldr1 (\x acc -> if x < acc then x else acc)          --alternate

largest :: [Int] -> Int
largest = foldl (\ac el-> if ac < el then el else ac) (minBound :: Int) 
-- largest = foldr1 (\x acc -> if x > acc then x else acc)      --alternate

-- If both recursion schemes are applicable, which one is preferable in terms of running time?
-- foldl' is tail recursive so foldl' is more space and time efficient than foldr.
