-- 1.
i :: a -> a
i = undefined

i2 :: a -> a
i2 x = x

-- 2.
c :: a -> b -> a
c x y = x

-- 3.
c'' :: b -> a -> b
c'' x y = x
-- this is like c.

-- 4.
c' :: a -> b -> b
c' x y = y

-- 5.
r :: [a] -> [a]
r x = []

-- 6.
co :: (b -> c) -> (a -> b) -> (a -> c)
co = (.)

-- 7.
a :: (a -> c) -> a -> a
a _ x = x

-- 8.
a' :: (a -> b) -> a -> b
a' f x = f x
