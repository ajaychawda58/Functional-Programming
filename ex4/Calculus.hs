module Calculus
where

infixl 6 :+:
infixl 7 :*:
infixl 8 :^:
infixl 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :^: Integer   -- power
  |  Function :.: Function  -- composition of functions
  deriving (Show)

--------------------------------------------------------------------------------
-- a)

apply :: Function -> (Rational -> Rational)
apply (Const r) = \x -> r
apply Id = \x -> x
apply (lf :+: rf) = \x -> (apply lf x) + (apply rf x) 
apply (lf :*: rf) = \x -> (apply lf x) * (apply rf x) 
apply (lf :^: po) = \x -> (apply lf x) ^^  po
apply (lf :.: rf) = \x -> (apply lf) ((apply) rf x) 

--------------------------------------------------------------------------------
-- b)

derive :: Function -> Function
derive (Const r) = Const 0
derive Id = Const 1
derive (lf :+: rf) = derive lf :+: derive rf
derive (lf :*: rf) = (lf :*: derive rf) :+: (derive lf :*: rf)
derive (lf :^: po) = (Const (toRational po)) :*: (lf :^: (po-1))  
derive (lf :.: rf) =  (derive lf :.: rf) :*: derive rf
{-
 - (x^3) o (4x)
 - ((3x^2) o (4x))* 4
 - 3 * 16 x^2
 - 192^x2
 -}
--------------------------------------------------------------------------------
-- c)
size :: Function -> Integer
size (Const _) = 1
size Id = 1
size (f1 :+: f2) = size f1 + size f2
size (f1 :*: f2) = size f1 + size f2
size (f1 :^: _) = size f1 + 1
size (f1 :.: f2) = size f1 + size f2

simplify :: Function -> Function
simplify Id = Id -- Identity, does not simplify anything. Do something useful here...
simplify (Const 0 :+: rf) = simplify rf
simplify (lf :+: Const 0 ) = simplify lf
simplify (Const 1 :*: rf) = simplify rf
simplify (lf :*: Const 1 ) = simplify lf
simplify (Const 1 :^: rf) = Const 1
simplify (lf :^: 1) = simplify lf
simplify (lf :^: 0) = Const 1
simplify (Const a :+: Const b) = Const (a+b)
simplify (Const a :+: Const b :+: f) = Const (a+b) :+: f
simplify (Const a :+: Const b :*: f) = Const (a+b) :*: f
simplify (Const a :*: Const b) = Const (a*b)
simplify (Id :.: rf) = simplify rf
simplify (lf :.: Id) = simplify lf
simplify id  = id 
-- Hint: The simplify tests first check whether the size for the given examples is
-- as small as possible. Have a look into the CalculusSpec.hs file, it contains
-- comments with the expected simplified solution for each example.
-- Then the simplified function is applied to check whether it is still correct.
-- You have to pass these "apply" tests (i.e. no simplification that leads to
-- wrong results), but the "size" test is not that important. Just try your best.
