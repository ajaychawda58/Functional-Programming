module TypeQuiz
where

-- a)
-- (Functor f) => f (a, b) -> f (b, a)
f1 = undefined


-- b)
-- (Functor f) => f (f (a, b)) -> f (f (a, b, b, a))
f2 = undefined


-- c)
-- (Functor f, Functor g) => f a -> g b -> f (g (b, a))
f3 = undefined


-- d)
-- (Applicative f) => f a -> f b -> f (a, b)
f4 = undefined


-- e)
-- (Applicative f) => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
f5 = undefined


-- f)
-- (Applicative f, Applicative g) => a -> f (g a)
f6 = undefined


-- g)
-- (Monad m) => a -> m (m a)
f7 = undefined


-- h)
-- (Monad m) => m a -> m (m a)
f8 = undefined


-- i)
-- (Monad m) => m (m (m a)) -> m a
f9 = undefined
