{-# LANGUAGE FlexibleInstances #-}

-- 1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where 
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b

-- 2.
data K a b = K a

instance Functor (K a) where 
    fmap _ (K a) = K a

-- 3. dafuq
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K2 a b = K2 a

instance Functor (Flip K2 a) where 
    fmap f (Flip (K2 x)) = Flip (K2 (f x))

-- 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where 
    fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where 
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where 
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9.
data List a = Nil | Cons a (List a)

instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where 
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3) 

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where 
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read sa) = Read $ fmap f sa
    -- or without fmap, as the book hints:
    -- fmap f (Read sa) = Read $ f . sa
