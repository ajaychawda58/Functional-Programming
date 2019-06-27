module Distribution
where

type Prob = Rational
newtype Dist event = D { fromD :: [(event, Prob)] } deriving (Show, Eq)

instance Functor Dist where
  fmap f (D d)    =  D [ (f e, p) | (e, p) <- d ]
instance Applicative Dist where
  pure a          =  D [(a, 1)]
  D fd <*> D xd   =  D [ (f x, p * q) | (f, p) <- fd, (x, q) <- xd ]
instance Monad Dist where
  return a        =  D [(a, 1)]
  D xd >>= k      =  D [ (y, p * q) | (x, p) <- xd, (y, q) <- fromD (k x) ]


-- a)
failureProb, successProb :: Prob
failureProb = 1/1000000 -- 0.0001%
successProb = 1 - failureProb

faultyAdd :: Integer -> Integer -> Dist Integer
faultyAdd x y = undefined


-- b)
gauss :: Integer -> Dist Integer
gauss = undefined


-- c)
fib :: Integer -> Dist Integer
fib = undefined
