-- 1.
-- []
-- pure :: a -> [a]
-- (<*>) :: [a->b] -> [a] -> [b]

-- 2.
-- IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3.
-- (,) a1
-- pure :: a -> (a1, a)
-- (<*>) :: (a1, a->b) -> (a1, a) -> (a1, b)

-- 4.
-- (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

-- Applicative laws:
-- 1. pure id <*> v = v
-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 3. pure f <*> pure x = pure (f x)
-- 4. u <*> pure y = pure ($ y) <*> u

data Pair a = Pair a a deriving Show

instance Functor Pair where 
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where 
    pure x = Pair x x
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)
    -- here, might it be possible to only use the first function on both values and still satisfy the applicative laws?

data Two a b = Two a b

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where 
    pure x = Two mempty x
    (<*>) (Two a b) (Two x y) = Two (a <> x) (b y)

data Three a b c = Three a b c

instance Functor (Three a b) where 
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (<*>) (Three a b c) (Three x y z) = Three (a <> x) (b <> y) (c $ z)

data Three' a b = Three' a b b

instance Functor (Three' a) where 
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where 
    pure x = Three' mempty x x
    (<*>) (Three' a b b') (Three' x y y') = Three' (a <> x) (b $ y) (b' $ y')

-- I should probably prove this sometime. or at least test it :)
