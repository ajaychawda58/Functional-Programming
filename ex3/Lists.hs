module Lists
where

prod :: [Integer] -> Integer
prod [] = 1
prod (x:xs) = x * prod xs

contains :: Integer -> [Integer] -> Bool
contains y [] = False
contains y (x:xs)
    | y == x = True
    | otherwise = contains y xs
--contains x y
--    | x `elem` y = True
--    | otherwise = False

nth :: Integer -> [a] -> Maybe a
nth _ [] = Nothing
nth y (x:xs)
    | (y == 0) = Just x 
    | otherwise = nth (y-1) xs

remove :: Integer -> [Integer] -> [Integer]
remove _ [] = []
remove x (y: xs)
    | x == y = remove x xs
    | otherwise = y : remove x xs


suffixes :: [a] -> [[a]]
--suffixes = tails
suffixes [] = [[]]
suffixes xs = xs : (suffixes $ tail xs)
