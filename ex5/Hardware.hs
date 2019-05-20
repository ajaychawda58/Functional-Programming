module Hardware
where
import Prelude hiding (and, or)

-- a)
mapr :: ((a, state) -> (b, state)) -> ([a], state) -> ([b], state)
mapr f (vals, state)  = 
    foldr (\e (as, s) -> let (na, ns) = f(e, s) in (na:as, ns)) ([], state) vals 

-- b)
data Bit  =  O | I deriving (Eq, Ord, Show)

and, or, xor :: Bit -> Bit -> Bit
and O _ =  O
and I b =  b
or  O b =  b
or  I _ =  I
xor O O =  O
xor O I =  I
xor I O =  I
xor I I =  O

type Carry = Bit
halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (b1, b2) = ((xor b1 b2), (and b1 b2))

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((b1, b2), c) = (ss, xor sc fc)
    where (fs, fc) = halfAdder(b1,b2); (ss, sc) = halfAdder(c, fs)

-- c)
rippleAdder :: ([Bit], [Bit], Carry) -> ([Bit], Carry)
rippleAdder (x, y, c) = mapr (\(p, q) -> fullAdder(p,q)) (pairs,  c)  
    where pairs = zip x y 
