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

-- Out of 16 boolean functions only 4 are associative with identity value
-- 1. AND
newtype AndAll = AndAll {fromAndAll :: Bool} deriving (Show)
instance Monoid AndAll where
    e = AndAll True
    (°) (AndAll x) (AndAll y) = AndAll (x && y)

-- 2. OR
newtype OrAll = OrAll {fromOrAll:: Bool} deriving (Show)
instance Monoid OrAll where
    e = OrAll False
    (°) (OrAll x) (OrAll y) = OrAll (x || y)

-- 3.XOR
newtype XorAll = XorAll {fromXorAll:: Bool} deriving (Show)
instance Monoid XorAll where
    e = XorAll False
    (°) (XorAll x) (XorAll y) = XorAll (x /= y)

-- 4. NXOR
newtype NxorAll = NxorAll {fromNxorAll:: Bool} deriving (Show)
instance Monoid NxorAll where
    e = NxorAll True
    (°) (NxorAll x) (NxorAll y) = NxorAll (x == y)

-- b)
{-

Please write your answer in this multiline comment.
1. In reduce, it check all values are True. Predefined method : and
2. In reduce, it check any of the value is True. Predefined method : or
3. In reduce, it check if the list has even number of True. 
4. In reduce, it check if the list has even number of False.

-}
