-- 2.3.a
-----------------------------------------------
-- There exist 4 common functions which have functions Bool -> Bool,
-- which are NOT, logical true, logical false and logical identity.

--usage of not

nott :: Bool -> Bool
nott x= do
    if x == False then True
    else False
{-  
alternate solution:
not :: Bool -> Bool
not False = True
not True  = False
-}

logicalTrue :: Bool -> Bool
logicalTrue x
    | x==False = True
    | otherwise = True

logicalFalse :: Bool -> Bool
logicalFalse x
    | x==True = False
    | otherwise = False

logicalIdentity :: Bool -> Bool     --output remains the same
logicalIdentity x
    | x==True = True
    | x==False = False

-- 2.3.b
-----------------------------------------------
-- For (Bool,Bool)-> Bool, there are many. 
-- For example, Definitions for 4 common functions AND, OR, XOR, XNOR.

--AND operator usage
aNNd :: (Bool,Bool) -> Bool
aNNd (x,y) = x && y

--OR operator usage
oRr :: (Bool,Bool) -> Bool
oRr (x,y) = x || y

--XOR
xor :: (Bool,Bool) -> Bool
xor (x,y)
        | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

--XNOR
xnor :: (Bool,Bool) -> Bool
xnor (x,y)
        | x == True && y == True = True
        | x == False && y == False = True
        | otherwise = False

-- 2.3.c
-----------------------------------------------
-- With function signature of Bool -> Bool -> Bool there exist many functions.
-- for example we have NOR, NAND and Boolean Implication operators

noR :: Bool -> Bool -> Bool
noR x y | x == True && y == False = False
        | x == False && y == True = False
        | x == True && y == True = False
        | x == False && y == False = True

nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

iMp :: Bool -> Bool -> Bool
iMp x y | x == True && y == True = True
        | x == True && y == False = False
        | x == False = True
{-
Boolean implication A implies B simply means "if A is true, 
then B must be true". This implies that if A isn't true, then B can be anything.
-}