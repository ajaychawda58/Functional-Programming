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
derive = undefined

--------------------------------------------------------------------------------
-- c)

simplify :: Function -> Function
simplify = id -- Identity, does not simplify anything. Do something useful here...

-- Hint: The simplify tests first check whether the size for the given examples is
-- as small as possible. Have a look into the CalculusSpec.hs file, it contains
-- comments with the expected simplified solution for each example.
-- Then the simplified function is applied to check whether it is still correct.
-- You have to pass these "apply" tests (i.e. no simplification that leads to
-- wrong results), but the "size" test is not that important. Just try your best.
