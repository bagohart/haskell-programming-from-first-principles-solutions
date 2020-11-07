{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans
import Control.Monad

-- ReaderT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where 
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT frma) = ReaderT $ (fmap . fmap) f frma
    -- which means:
    -- fmap f (ReaderT frma) = ReaderT $ (\r -> f <$> frma r)

instance (Applicative m) => Applicative (ReaderT r m) where 
    pure x = ReaderT $ \r -> pure x
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (<*>) (ReaderT rmab) (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma
    -- ^ this is just copy and paste. but to see what this actually means, this should be equivalent to:
    -- (<*>) (ReaderT rmab) (ReaderT rma) = ReaderT $ \r -> rmab r <*> rma r

instance (Monad m) => Monad (ReaderT r m ) where 
    return = pure
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    -- (>>=) (ReaderT rma) f = ReaderT $
    --     \r -> rma r >>= (\a -> (runReaderT (f a)) r)
    -- or with do syntax:
    (>>=) (ReaderT rma) f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r
        
-----
instance MonadTrans (ReaderT r) where 
    lift :: (Monad m) => m a -> ReaderT r m a
    lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where 
    liftIO :: IO a -> ReaderT r m a
    liftIO = lift . liftIO

------ MaybeT

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where 
    fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where 
    pure = MaybeT . pure . pure
    (<*>) (MaybeT fab) (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where 
    return = pure
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (>>=) (MaybeT mma) f =
        MaybeT $ mma >>= (\ma -> case ma of
                             Nothing -> return Nothing
                             (Just a) -> runMaybeT (f a))

    -- MaybeT m a ~> m (Maybe a) >>= (ma -> m (Maybe b)) ~> m (Maybe b) ~> MaybeT m b

instance MonadTrans MaybeT where 
    lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where 
    liftIO :: IO a -> MaybeT m a
    liftIO = lift . liftIO
    -- inlined definition of lift:
    -- liftIO = (MaybeT . fmap Just) . liftIO

-- Reader
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where 
    fmap f (Reader r) = Reader $ f . r


instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)


instance Monad (Reader r) where 
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

---- so far copied from old things, now new things

-- ReaderT r Maybe  vs  MaybeT (Reader r)

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- and
-- newtype Reader r a = Reader { runReader :: r -> a }


-- Compare:
-- (ReaderT r Maybe a) is a wrapper around (r -> Maybe a)
-- (MaybeT (Reader r)) is a wrapper around m (Maybe a) ~> (Reader r) (Maybe a) is a wrapper around (r -> Maybe a)
-- so structurally, they seem to be the same thing.
-- This does not yet mean that they have the same behaviour, though. Let's see...

type RTM r a = ReaderT r Maybe a
type MTR r a = MaybeT (Reader r) a

x = ReaderT $ const (Just 1)
x' = MaybeT $ const (Just 1)
-- ... ok, I won't get at a difference like this o_O
y = (+1) <$> x
y' = (+1) <$> x'
-- it remains identical.
z = pure (+) <*> x <*> y
z' = pure (+) <*> x' <*> y'
-- still...
w = do
    a <- x
    return (a+4)

w' = do
    a <- x'
    return (a+4)
-- no difference. To prove this, I would have to show that the implementation of the combined monad in this case would be identical. This should be as easy as inlining both implementations.
-- If it's not completely broken, the implementation of <$> and <*> should fall out ouf >>=, so it should suffice to show that >>= is identical. But let's do return first:

-- return :: a -> RTM a
-- return x = pure x
-- = ReaderT $ \r -> pure x
-- = ReaderT $ \r -> Just x

-- return :: a -> MTR a
-- return x = pure x
-- = MaybeT . pure . pure $ x
-- = MaybeT (pure (pure x))
-- = MaybeT (pure (Just x))
-- = MaybeT (Reader $ const (Just x))
-- = MaybeT (Reader $ \r -> Just x)

-- so the final representation ends up different, but the value/function is the same.

-- (>>=) :: RTM a -> (a -> RTM b) -> RTM b
-- (>>=) (ReaderT rma) f = 
--           ReaderT $ \r -> do
--                a <- rma r
--                runReaderT (f a) r
--  the do notation is syntactic sugar for >>=, and rma r :: m a = Maybe a, so this uses >>= for Maybe:
-- (>>=) (ReaderT rma) f = 
--           ReaderT $ \r -> do
--                  case (r rma) of
--                      Nothing -> Nothing
--                      (Just a) -> runReaderT (f a) r
--
-- and
-- (>>=) :: MTR a -> (a -> MTR b) -> MTR b
-- (>>=) (MaybeT rma) f = MaybeT $ rma >>= (\ma -> case ma of
--                           Nothing -> return Nothing
--                           (Just a) -> runMaybeT (f a))
--  now inline the definition of >>= for Reader
--  colloquially:
--  - run rma on r to get ma.
--  - lift f over ma to get mrmb
--  - lift r over m and apply rmb to get mmb
--  - join to mb
--  or something o_O
-- (>>=) (MaybeT (Reader rma)) aRmb = MaybeT $ Reader $ \r -> case (rma r) of
--                                                                  Nothing -> Nothing
--                                                                  (Just (MaybeT (Reader rmb))) -> rmb r
--  uh uh. this is confusing u_U
--  but looks about right x_X
