module HigherOrder
where

f :: [(Bool, a)] -> [a]
f = undefined

g :: [(a, b)] -> [(b, a)]
--g = undefined
g ((x,y): xs) = (y,x):g xs


h :: [(Integer, a)] -> [a]
h = undefined
