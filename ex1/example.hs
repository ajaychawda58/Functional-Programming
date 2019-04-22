-- compute the square of an integer
square :: Integer -> Integer
square x = x * x
-- smaller of two arguments
smaller :: (Integer, Integer) -> Integer
smaller (x, y) = if x <= y then x else y