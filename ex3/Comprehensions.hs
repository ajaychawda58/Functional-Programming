module Comprehensions
where

f :: [(Bool, a)] -> [a]
--f = undefined
f [] = []
f xs= [y | (x,y)<- xs, x == True]

g :: [(a, b)] -> [(b, a)]
g = undefined

h :: [(Integer, a)] -> [a]
h = undefined
