module Hardware
where
import Prelude hiding (and, or)

-- a)
mapr :: ((a, state) -> (b, state)) -> ([a], state) -> ([b], state)
mapr = undefined


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
halfAdder = undefined

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder = undefined


-- c)
rippleAdder :: ([Bit], [Bit], Carry) -> ([Bit], Carry)
rippleAdder = undefined
