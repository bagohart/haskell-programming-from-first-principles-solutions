import Data.Foldable (fold)

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    -- equivalent to:
    -- pure x = Compose $ pure (pure x)
    -- or also
    -- pure = Compose . (pure . pure)

    (<*>) ::
        Compose f g (a -> b) ->
        Compose f g a ->
        Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

-- equivalent to:
-- (Compose f) <*> (Compose a) = Compose $ pure (<*>) <*> f <*> a

{-
instance (Monad f, Monad g) => monad (Compose f g) where
    (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
    (>>=) = ??? -- impossible!
-}

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = foldMap (foldMap f) fga

-- general reminder:
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: (Applicative h) => (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

-- this works. lol. but not obvious why. last time I came up with a wacky combination of
-- sequenceA and fmap, and at least a reason why it worked, this time it was just guessing.
-- Can I deduce this after the fact somehow?
-- traverse _ fga -- this works on the outer f
-- _ :: g a -> h (g b)
-- traverse f :: g a -> h (g b)
-- ... I guess it matches.
