import Control.Monad (join)

-- I can combine two monads like this:
newtype MaybeIO a = MaybeIO {runMaybeIO :: IO (Maybe a)}
newtype MaybeList a = MaybeList {runMaybeList :: [Maybe a]}

-- but apparently this is "Doing it badly"

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)
newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative m) => Applicative (IdentityT m) where
    pure :: a -> IdentityT m a
    pure = IdentityT . pure
    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    IdentityT fab <*> IdentityT fa = IdentityT $ fab <*> fa

instance Monad Identity where
    return = pure
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity a) >>= amb = amb a

-- this works, but below I'll do it again, guided by the book
-- instance (Monad m) => Monad (IdentityT m) where
--     return = pure
--     (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--     (IdentityT ma) >>= aImb = IdentityT $ ma >>= (runIdentityT . aImb)
--     -- Or:
--     -- (IdentityT ma) >>= f = let
--     --             x = do
--     --                 a <- ma
--     --                 runIdentityT $ f a
--     --             -- x :: m b
--     --            in IdentityT x
--     -- Or ... (approximately my old solution)
--     -- (IdentityT ma) >>= aImb = IdentityT $ join (runIdentityT <$> (aImb <$> ma))

-- approach guided by the book (not  bad!):
-- instance (Monad m) => Monad (IdentityT m) where
--     return = pure
--     (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--     (IdentityT ma) >>= f =
--         -- let aimb :: a
--             let aimb = join (fmap runIdentityT (fmap f ma))
--         in IdentityT aimb

-- approach guided by the book, final refactoring:
instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        -- let aimb :: a
            -- let aimb = join (fmap (runIdentityT . f) ma)
            let aimb = ma >>= runIdentityT . f
        in IdentityT aimb

-- Equivalent implementation in the `transformers` library
-- m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m
-- the rightmost `runIdentityT m` does the pattern matching
-- =<< is just `flip >>=`, so it is equivalent to
-- IdentityT $ (runIdentityT m) >>= (runIdentityT . k)
-- which is pretty much identical
--
-- TODO: anki karte für IdentityT implementierung bauen.
