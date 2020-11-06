{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans
import Control.Monad

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
