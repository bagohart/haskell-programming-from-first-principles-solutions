{-# LANGUAGE Strict #-}

data List a = Nil | Cons a (List a) deriving (Show)
-- data List a = Nil | Cons a ~(List a) deriving (Show)
-- data List a = Nil | Cons ~a ~(List a) deriving (Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' x = xs where xs = (Cons x xs)

main = do
    print $ take' 10 $ map' (+1) (repeat' 1)
    -- this... doesn't terminate. Because repeat' creates a strict infinite list?
    -- add a ~ before the ~(List a) to make it terminate.

twoEls = Cons 1 (Cons undefined Nil)
oneEl = take' 1 twoEls
three = Cons 2 twoEls
oneElT = take' 1 three
