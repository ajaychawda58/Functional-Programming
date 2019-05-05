module HigherOrder
where

f :: [(Bool, a)] -> [a]
f xs = map (\(x, y)-> y) (filter (\(x, y) -> x) xs)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

g :: [(a, b)] -> [(b, a)]
--g = undefinedg
{--g [] = []
--g ((x,y): xs) = (y,x):g xs            
g [] = []
g ((a,b):xs) = map swap [(a,b)]
--}
g xs = map swap xs

expand :: (Integer, a) -> [a] 
expand (0 , y) = []
expand (x,y) = y: expand (x-1, y) 

h :: [(Integer, a)] -> [a]
h xs = concat (map expand xs)
