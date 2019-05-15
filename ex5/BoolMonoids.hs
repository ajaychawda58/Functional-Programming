module BoolMonoids
where
import Prelude hiding (Monoid)

{-
See chapter 10 in the lecture.
We use slightly different symbols here, as the unicode symbols are hard to type.
Feel free to change it back to unicode syntax if you prefer it.
-}

class Monoid a where
    e   :: a
    (°) :: a -> a -> a

reduce :: (Monoid m) => [m] -> m
reduce [] = e
reduce (x : xs) = x ° reduce xs


-- Example definition from the lecture, adapted to ascii syntax:
newtype Additive = Sum {fromSum :: Int} deriving (Show)
instance Monoid Additive where
    e = Sum 0
    x ° y = Sum (fromSum x + fromSum y)


-- a)
-- Please write your newtype and instance definitions here...


-- b)
{-

Please write your answer in this multiline comment.

-}
