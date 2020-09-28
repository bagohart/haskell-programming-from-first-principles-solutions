{-# LANGUAGE FlexibleContexts #-}

-- 1. Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity x) = Identity $ f x

instance Foldable Identity where 
    foldr f e (Identity x) = f x e

instance Traversable Identity where 
    -- sequenceA :: (Applicative f) => Identity (f a) -> f (Identity a)
    sequenceA (Identity fa) = Identity <$> fa
    -- traverse :: (a -> f b) -> Identity (f a) -> f (Identity b)
    traverse f (Identity fa) = Identity <$> (f fa)

-- 2. Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Show)

instance Functor (Constant a) where 
    fmap _ (Constant x) = (Constant x)

instance Monoid a => Applicative (Constant a) where
    pure x = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant $ x <> y

instance Foldable (Constant a) where 
    foldr _ e (Constant x) = e

instance Traversable (Constant a) where
    -- sequenceA :: Applicative f => Constant (f a) -> f (Constant a)
    sequenceA (Constant x) = pure (Constant x)
    -- traverse :: (a -> f b) -> Constant (f a) -> f (Constant b)
    traverse f (Constant x) = pure (Constant x)

-- 3. Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where 
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep $ f x

instance Foldable Optional where 
    foldr _ e Nada = e
    foldr f e (Yep x) = f x e

instance Traversable Optional where 
    -- sequenceA :: Applicative f => Optional (f a) -> f (Optional a)
    sequenceA Nada = pure Nada
    sequenceA (Yep fa) = Yep <$> fa
    -- traverse :: (a -> f b) -> Optional (f a) -> f (Optional b)
    traverse f Nada = pure Nada
    traverse f (Yep a) = Yep <$> (f a)

-- 4. List
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

instance Monad List where 
    return = pure
    (>>=) = flip flatMap -- look mum, pointfree
    -- or just
    -- (>>=) xs f = flatMap f xs

instance Foldable List where 
    foldr _ e Nil = e
    foldr f e (Cons x xs) = f x (foldr f e xs)

-- idea: go from [f a] to [f [a]], and then fold the outer list, using applicative to (++) all the lists together
instance Traversable List where 
    -- sequenceA :: Applicative f => List (f a) -> f (List a)
    sequenceA Nil = pure Nil
    sequenceA xs = fold (\x ys -> append <$> (oneL <$> x) <*> ys) (pure Nil) xs
    -- traverse :: (a -> f b) -> List (f a) -> f (List b)
    traverse f xs = fold (\x ys -> append <$> (oneL <$> (f x)) <*> ys) (pure Nil) xs

oneL :: a -> List a
oneL x = Cons x Nil

-- 5. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where 
    pure x = Three mempty mempty x
    (<*>) (Three a b fc) (Three a' b' c) = Three (a<>a') (b<>b') (fc c)

instance Foldable (Three a b) where 
    foldr f e (Three a b c) = f c e

instance Traversable (Three a b) where 
    -- sequenceA :: Applicative f => Three (f c) -> f (Three c)
    sequenceA (Three a b fc) = (Three a b) <$> fc
    -- traverse :: (a -> f b) -> Three (f a) -> f (Three b)
    traverse f (Three a b fc) = (Three a b) <$> (f fc)


-- 5. Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where 
    fmap f (Pair x y) = Pair x (f y)

instance (Monoid a) => Applicative (Pair a) where 
    pure x = Pair mempty x
    (<*>) (Pair x fy) (Pair x' y) = Pair (x<>x') (fy y)

instance Foldable (Pair a) where 
    foldr f e (Pair x y) = f y e

instance Traversable (Pair a) where
    -- sequenceA :: Applicative f => Pair (f c) -> f (Pair c)
    sequenceA (Pair a fb) = (Pair a) <$> fb
    -- traverse :: (a -> f b) -> Pair (f a) -> f (Pair b)
    traverse f (Pair a b) = (Pair a) <$> (f b)


-- 6.
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where 
    fmap f (Big x y y') = Big x (f y) (f y')

instance (Monoid a) => Applicative (Big a) where 
    pure x = Big mempty x x
    (<*>) (Big a fb1 fb2) (Big a' b1 b2) = Big (a<>a') (fb1 b1) (fb2 b2)

instance Foldable (Big a) where 
    foldr f e (Big x y y') = f y (f y' e)

-- Big "lol" (Just 1) (Just 2) ~> Just (Big "lol" 1 2)
instance Traversable (Big a) where 
    -- sequenceA :: Applicative f => Big (f c) -> f (Big c)
    sequenceA (Big x fb fb') = (Big x) <$> fb <*> fb'
    -- traverse :: (a -> f b) -> Big (f a) -> f (Big b)
    traverse f (Big x a a') = (Big x) <$> (f a) <*> (f a')

-- 7.
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where 
    fmap f (Bigger x y y' y'') = Bigger x (f y) (f y') (f y'')

instance (Monoid a) => Applicative (Bigger a) where 
    pure x = Bigger mempty x x x
    (<*>) (Bigger a fb1 fb2 fb3) (Bigger a' b1 b2 b3) = Bigger (a<>a') (fb1 b1) (fb2 b2) (fb3 b3)

instance Foldable (Bigger a) where 
    foldr f e (Bigger x y y' y'') = f y (f y' (f y'' e))

-- Bigger "lol" (Just 1) (Just 2) (Just 3) ~> Just (Bigger "lol" 1 2 3)
instance Traversable (Bigger a) where 
    -- sequenceA :: Applicative f => Bigger (f c) -> f (Bigger c)
    sequenceA (Bigger x fb fb' fb'') = (Bigger x) <$> fb <*> fb' <*> fb''
    -- traverse :: (a -> f b) -> Bigger (f a) -> f (Bigger b)
    traverse f (Bigger x a a' a'') = (Bigger x) <$> (f a) <*> (f a') <*> (f a'')
    
-- S
-- this means it has something of type a (the second) and the first thing expects a type
-- for instance, S [1] 1
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where 
    fmap f (S na a) = S (f <$> na) (f a)

instance (Applicative n) => Applicative (S n) where 
    pure a = S (pure a) a
    (<*>) (S nfa fa) (S na a) = S (nfa <*> na) (fa a)

instance (Foldable n) => Foldable (S n) where 
    foldMap f (S na a) = (foldMap f na) <> (f a)
    foldr f e (S na a) = f a (foldr f e na)
    
-- the n (f a) ~> f (n a) is given, so
-- (S nfa fa) ~> (S fna fa) ~> lift the S to get it into fna and fa ~> f (S na a)
instance (Traversable n) => Traversable (S n) where 
    -- sequenceA :: Applicative f => (S (n (f a)) (f a)) -> f (S (n a) a)
    sequenceA (S nfa fa) = S <$> (sequenceA nfa) <*> fa
    -- traverse :: (a -> f b) -> S (n a) a -> f (S (n b) b)
    traverse f (S na a) = S <$> (sequenceA (f <$> na)) <*> (f a)
    
-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where 
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where 
    foldMap f Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node l a r) = (foldMap f l) <> (f a) <> (foldMap f r)

-- Node Empty (Just 1) (Leaf $ Just 2) ~> Just $ Node Empty 1 (Leaf 2)
-- Node (Leaf $ Just 5) (Just 1) (Leaf $ Just 2) ~> Just (Node (Leaf 5) 1 (Leaf 2))
instance Traversable Tree where 
    -- sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
    sequenceA Empty = pure Empty
    sequenceA (Leaf fa) = Leaf <$> fa
    sequenceA (Node lfa fa rfa) = Node <$> (sequenceA lfa) <*> fa <*> (sequenceA rfa)
    -- traverse :: (a -> f b) -> Tree a -> f (Tree b)
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> (f a)
    traverse f (Node la a ra) = Node <$> (traverse f la) <*> (f a) <*> (traverse f ra)

