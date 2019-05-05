module Comprehensions
where

f :: [(Bool, a)] -> [a]
--f = undefined
f [] = []
f xs= [y | (x,y)<- xs, x == True]

g :: [(a, b)] -> [(b, a)]
--g = undefined
g [] = []
g xs = [(p,q) | (x,y) <- xs, p <- [y], q <- [x]]

h :: [(Integer, a)] -> [a]
--h = undefined
--h xs = concat[y | (x,y) <- xs, x<-[1..x]]
--h xs = return.concat $ [y | (x,y) <- xs, x<-[1..x]]
h xs = [ y | (x,y) <- xs, x<-[1..x]]    -- this one works though
