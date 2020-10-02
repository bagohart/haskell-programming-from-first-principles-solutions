{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

instance Functor Identity where 
    fmap f (Identity x) = Identity $ f x

-- Identity :k: * -> *
-- id :: a -> a


newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)
-- Compose :k: (* -> *) -> (* -> *) -> * -> *
-- (.) :: (b -> c) -> (a -> b) -> a -> c

instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- "GOTCHA!"
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

-- -- impossible twonad
-- instance (Monad f, Monad g) => Monad (Compose f g) where 
--     return = pure

--     (>>=) :: Compose f g a -> (a -> f g b) -> f g b
--     (>>=) (Compose fga) aFgb = ...
--         f g a ~> f g (f g b) ~> :'(
