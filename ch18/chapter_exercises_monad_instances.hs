-- 1.
data Nope a = NopeDotJpg

instance Functor Nope where 
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where 
    pure _ = NopeDotJpg
    (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where 
    return = pure
    (>>=) NopeDotJpg _ = NopeDotJpg

-- 2.
data PhhhbbttttEither b a = Left' a | Right' b

instance Functor (PhhhbbttttEither b) where 
    fmap f (Left' x) = Left' $ f x
    fmap _ (Right' x) = Right' x

instance Applicative (PhhhbbttttEither b) where 
    pure x = Left' x
    (<*>) (Right' x) (Right' y) = Right' x -- (there is no monad for Validation)
    (<*>) (Right' x) (Left' _) = Right' x
    (<*>) (Left' _) (Right' x) = Right' x
    (<*>) (Left' f) (Left' y) = Left' $ f y

instance Monad (PhhhbbttttEither b) where
    return = pure
    (>>=) (Right' x) _ = Right' x
    (>>=) (Left' x) f = f x


-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where 
    pure x = Identity x
    (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where 
    return = pure
    (>>=) (Identity x) f = f x

-- 4.
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

-- (previous part on List copied from last chapter)
instance Monad List where 
    return = pure
    (>>=) = flip flatMap -- look mum, pointfree
    -- or just
    -- (>>=) xs f = flatMap f xs
