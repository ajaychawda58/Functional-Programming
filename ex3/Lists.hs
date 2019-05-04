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
--nth = undefined
nth _ [] = Nothing
nth 1 (x: _) = Just x
nth n (_: xs) = nth (n-1) xs

remove :: Integer -> [Integer] -> [Integer]
remove = undefined

suffixes :: [a] -> [[a]]
suffixes = undefined
