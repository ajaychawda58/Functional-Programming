module HigherOrder
where

f :: [(Bool, a)] -> [a]
f = undefined

g :: [(a, b)] -> [(b, a)]
--g = undefinedg
g [] = []
g ((x,y): xs) = (y,x):g xs


h :: [(Integer, a)] -> [a]
h = undefined
