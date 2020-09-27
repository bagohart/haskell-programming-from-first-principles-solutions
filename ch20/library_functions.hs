import Data.Monoid

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (==x))

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = foldr (\a as -> a == x || as) False

-- 4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = undefined
-- to do this I would have to define a new Monoid that always takes the minimum and has nothing as mempty
-- possible, but overkill.

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' = foldr op Nothing
    where op x Nothing = Just x
          op x (Just y) = Just $ min x y

-- 5.
maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' = foldr op Nothing
    where op x Nothing = Just x
          op x (Just y) = Just $ max x y


-- 6.
null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (\_ -> All False)

null'' :: (Foldable t) => t a -> Bool
null'' = foldr (\_ xs -> True || xs) False


-- 7.
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

length'' :: (Foldable t) => t a -> Int
length'' = foldr (\t ts -> 1 + ts) 0

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (:[])

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldr (:) []
-- this looks like I'm replacing a list... with a list o_O

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldr (<>) mempty

-- 10.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f)  mempty
-- or without pointfree voodoo:
-- foldMap' f = foldr (\x xs -> f x <> xs) mempty
