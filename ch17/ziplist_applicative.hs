data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where 
    pure f = Cons f Nil
    (<*>) Nil Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) xs ys = flatMap (\f -> fmap f ys) xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . (fmap f)

--- so far copy from list_applicative.hs

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zip' :: List a -> List b -> List (a,b)
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons x xs) (Cons y ys) = Cons (x,y) (zip' xs ys)

zipWith' :: (a -> b -> c) -> List a -> List b -> List (c)
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where 
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
    pure f = ZipList' $ repeat' f
    (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $ zipWith' ($) xs ys
    -- or:
    -- (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $ fmap (\(f, y) -> f y) (zip' xs ys)
