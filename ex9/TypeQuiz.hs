module TypeQuiz
where

-- a)
-- (Functor f) => f (a, b) -> f (b, a)
f1 x = fmap (\(a, b) -> (b, a)) x


-- b)
-- (Functor f) => f (f (a, b)) -> f (f (a, b, b, a))
f2 x = fmap (\f -> (fmap (\(a,b) -> (a,b,b,a) )f)) x


-- c)
-- (Functor f, Functor g) => f a -> g b -> f (g (b, a))
f3 f g = fmap (\a -> (fmap (\b -> (b, a)) g)) f


-- d)
-- (Applicative f) => f a -> f b -> f (a, b)
f4 x y = (\a b -> (a,b)) <$> x <*> y


-- e)
-- (Applicative f) => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
f5 f fa fb fc = f <$> fa <*> fb <*> fc


-- f)
-- (Applicative f, Applicative g) => a -> f (g a)
f6 a = pure (pure a)

-- g)
-- (Monad m) => a -> m (m a)
f7 a = return (return a)


-- h)
-- (Monad m) => m a -> m (m a)
f8 a = return a


-- i)
-- (Monad m) => m (m (m a)) -> m a
f9 a = a >>= id >>= id  
