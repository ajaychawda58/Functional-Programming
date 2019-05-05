module HigherOrder
where

f :: [(Bool, a)] -> [a]
f = undefined

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

g :: [(a, b)] -> [(b, a)]
--g = undefinedg
{--g [] = []
--g ((x,y): xs) = (y,x):g xs            --for some reason not passes testcases but works perfectly in mine
g [] = []
g ((a,b):xs) = map swap [(a,b)]
--}
g xs = map swap xs

prnt :: (Integer,a) -> a
prnt (x,y) = concat [y | n<-[0..x-1]]

h :: [(Integer, a)] -> [a]
--h = undefined
h xs = map prnt xs
