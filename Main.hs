{-# LANGUAGE RankNTypes #-}
module Main where

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (flip (,) x) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = ((,) x) <$> f a

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = l where
    l f s = (set s) <$> (f $ get s)

-- Combine 2 lenses to make a lens which works on Either.
--choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
--         -> Lens (Either s1 s2) (Either t1 t2) a b
--choosing l1 l2 = _
--
---- Modify the target of a lens and return the result. (Bonus points if you
---- do it without lambdas and defining new functions.)
--(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
--(<%~) l f s = _
--
---- Modify the target of a lens, but return the old value.
--(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
--(<<%~) l f s = _
--
---- There's a () in every value. (No idea what this one is for, maybe it'll
---- become clear later.)
--united :: Lens' a ()
--
--united = _

main :: IO ()
main = print "lols"
