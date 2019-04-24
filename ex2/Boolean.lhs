--Can you please implement functions for (Bool,Bool) -> Bool. I struggled to implement this one and please correct my mistakes or make it better if you like.

--There are 4 functions which have functions Bool -> Bool
--some of them are not, errorToTrue, errorToFalse, mkTablesNextToCode.

--usage of not

nott :: Bool -> Bool
nott x= do
    if x == False then True
    else False
{-  alternate solution
    not :: Bool -> Bool
    not False = True
    not True  = False
-}

-- For Bool -> Bool -> Bool, there are around 4 functions and, or, xor, nand etc.

--AND operator usage
aNd :: Bool->Bool -> Bool
aNd x y | x == True && y == True = True
        | otherwise = False

--OR operator usage

oR :: Bool -> Bool -> Bool
oR x y | x == True && y == False = True
       | x == False && y == True = True
       | x == True && y == True = True
       | otherwise = False


--xor

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False
