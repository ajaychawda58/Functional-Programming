module HigherOrder
where

f :: [(Bool, a)] -> [a]
f = undefined

swap :: (Integer,Integer) -> (Integer,Integer)
swap (x,y) = (y,x)

g :: [(a, b)] -> [(b, a)]
--g = undefinedg
--g [] = []
--g ((x,y): xs) = (y,x):g xs
g [] = []
g ((x,y):xs) = map swap [(x,y)]


h :: [(Integer, a)] -> [a]
h = undefined
