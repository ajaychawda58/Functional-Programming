module HigherOrder
where

f :: [(Bool, a)] -> [a]
f = undefined

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

g :: [(a, b)] -> [(b, a)]
--g = undefinedg
--g [] = []
--g ((x,y): xs) = (y,x):g xs
g [] = []
g ((a,b):xs) = map swap [(a,b)]


h :: [(Integer, a)] -> [a]
h = undefined
