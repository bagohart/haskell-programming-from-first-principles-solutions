-- 1.
data Constant a b = Constant b

instance Foldable (Constant a) where 
    foldr f e (Constant x) = f x e

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where 
    foldr f e (Two x y) = f y e

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where 
    foldr f e (Three a b c) = f c e

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where 
    foldr f e (Three' a b b') = f b (f b' e)

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where 
    foldr f e (Four' a b1 b2 b3) = foldr f e [b1, b2, b3]

-- "thinking cap time"
-- I can do this:
-- turn the t thing into a list with toList
-- filter the list
-- then I can pure them into being an applicative, i.e. [f a]
-- then I can fold the list with <>
-- or just use foldMap to do that in one step
-- where the tricky thing remains to filter out the things, but hey, I have mempty to do that?
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

