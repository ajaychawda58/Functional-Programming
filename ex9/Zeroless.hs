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
size = undefined

-- b)
cons :: elem -> Sequ elem -> Sequ elem
cons = undefined

-- c)
head :: Sequ elem -> elem
head = undefined
