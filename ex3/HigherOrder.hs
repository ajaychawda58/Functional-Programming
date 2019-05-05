module HigherOrder
where

f :: [(Bool, a)] -> [a]
f xs = map (\(x, y)-> y) (filter (\(x, y) -> x) xs)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

g :: [(a, b)] -> [(b, a)]
g xs = map swap xs

expand :: (Integer, a) -> [a]
expand (x, y)
  | x > 0 = y: expand (x-1, y)
  |otherwise = []

h :: [(Integer, a)] -> [a]
h xs = concat (map expand xs)
