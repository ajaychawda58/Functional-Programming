module Comprehensions
where

f :: [(Bool, a)] -> [a]
--f = undefined
f [] = []
f xs= [y | (x,y)<- xs, x == True]

g :: [(a, b)] -> [(b, a)]
--g = undefined
g [] = []
g xs = [(x,y) | (x,y) <-xs, let p=x,let q=y,let x=q,let y=p]

h :: [(Integer, a)] -> [a]
--h = undefined
--h xs = concat[y | (x,y) <- xs, x<-[1..x]]
--h xs = return.concat $ [y | (x,y) <- xs, x<-[1..x]]
h xs = [ y | (x,y) <- xs, x<-[1..x]]
