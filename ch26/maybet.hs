{-# LANGUAGE InstanceSigs #-}

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
