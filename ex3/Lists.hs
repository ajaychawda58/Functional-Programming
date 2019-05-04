module Lists
where

prod :: [Integer] -> Integer
prod [] = 1
prod (x:xs) = x * prod xs

contains :: Integer -> [Integer] -> Bool
contains = undefined

nth :: Integer -> [a] -> Maybe a
nth = undefined

remove :: Integer -> [Integer] -> [Integer]
remove = undefined

suffixes :: [a] -> [[a]]
suffixes = undefined
