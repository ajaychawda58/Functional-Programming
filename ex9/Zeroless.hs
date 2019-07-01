module Zeroless
where

type Pair elem = (elem, elem)

data Sequ elem
  = Nil
  | One elem      (Sequ (Pair elem))
  | Two elem elem (Sequ (Pair elem))
  deriving (Eq, Show)

-- a)
size :: Sequ elem -> Integer
--size = undefined
size (Nil) = 0
size (One n) = 1 + 2*size n
size (Two a n) = 2 + 2*size n

-- b)
cons :: elem -> Sequ elem -> Sequ elem
--cons = undefined
cons a b = case b of
  Nil    -> One a Nil
  One y ys -> Two $ cons (Pair a y) ys
  Two (One y ys) -> One $ (Two $ cons (Pair a y ) ys)
{-
0
1
2
11
12
21
22
111
112
121
.
.
-}
--cons will add element while head will show the first element
-- c)
check :: Sequ elem -> Maybe elem
check a = case a of
  Nil    -> Nothing
  One y ys -> Just y
  Two   ys -> fst <$> check ys

head :: Sequ elem -> elem
--head = undefined
head a = case check a of
  Just y  -> y
  Nothing -> error "Empty Head"

{-(<$>) :: Functor f => (a->b) -> f a -> f b-}