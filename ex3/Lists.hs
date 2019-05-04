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
--contains x l
--    | x `elem` l = True
--    | otherwise = False

nth :: Integer -> [a] -> Maybe a
nth = undefined

remove :: Integer -> [Integer] -> [Integer]
remove = undefined

suffixes :: [a] -> [[a]]
suffixes = undefined
