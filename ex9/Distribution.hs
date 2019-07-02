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

addDist :: Dist (Integer ->Integer -> Integer)
addDist = D[((\x y -> 0), failureProb),((+), successProb)]

faultyAdd :: Integer -> Integer -> Dist Integer
faultyAdd x y = fmap (\f -> f x y) addDist


-- b)
gauss :: Integer -> Dist Integer
gauss 0 = pure 0
gauss n = do
    res <- gauss(n-1)
    faultyAdd n res


-- c)
fib :: Integer -> Dist Integer
fib 0 = pure 0
fib 1 = pure 1
fib n = do
    res <- fib (n-2)
    res1 <- fib (n-1)
    faultyAdd res res1
