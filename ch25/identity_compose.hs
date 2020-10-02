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

v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

instance (Foldable f, Foldable g) => Foldable (Compose f g) where 
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = foldMap (foldMap f) fga
    -- Functor is not a superclass of Foldable, so I can't use fmap to accomplish this.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where 
    traverse :: (Applicative w) => (a -> w b) -> Compose f g a -> w (Compose f g b)
    traverse awb (Compose fga) =  fmap Compose $ sequenceA $ fmap (traverse awb) fga
    --      f (g a) # fmap (traverse awb)
    -- ~>   f (w (g b)) # traverse id
    -- ~>   w (f (g b)) # fmap Compose
    -- ~>   w (Compose f g b)
