{-# LANGUAGE InstanceSigs #-}

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where 
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    -- fmap f (StateT smas) = StateT $ \s -> fmap (\(a,s) -> (f a, s)) (smas s)
    -- which should be equivalent to:
    fmap f (StateT smas) = StateT $ fmap (fmap (\(a,s) -> (f a, s))) smas

instance (Monad m) => Applicative (StateT s m) where 
    pure :: a -> StateT s m a
    pure x = StateT $ \s -> pure (x,s)
    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    -- (<*>) (StateT smabs) (StateT smas) = StateT $ \s ->
    --     (smabs s) >>= (\(fab,s') -> fmap (\(a,s'') -> (fab a,s'')) (smas s))
    -- or, with do syntax, disguising that the second thing is merely fmap:
    (<*>) (StateT smabs) (StateT smas) = StateT $ \s -> do
        (fab, s') <- smabs s
        (a, s'') <- smas s'
        return (fab a, s'')

instance (Monad m) => Monad (StateT s m) where 
    return = pure
    (>>=) :: (StateT s m a) -> (a -> StateT s m b) -> StateT s m b
    (>>=) (StateT smas) f = StateT $ \s -> do
        (a,s') <- smas s
        (b,s'') <- (runStateT $ f a) s'
        return (b,s'')
