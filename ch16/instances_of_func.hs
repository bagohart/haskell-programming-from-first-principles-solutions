newtype Identity a = Identity a

instance Functor Identity where 
    fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a

instance Functor Pair where 
    fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where 
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b

instance Functor (Three' a) where 
    fmap f (Three' x y z) = Three' x (f y) (f z)

-- Four is analogous and boring.
-- Trivial won't work, because it doesn't have kind * -> *
