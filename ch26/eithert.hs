{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where 
    fmap f (EitherT meea) = EitherT $ (fmap . fmap) f meea

instance (Applicative m) => Applicative (EitherT e m) where 
    pure :: a -> EitherT e m a
    pure x = EitherT $ pure (Right x)
    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (<*>) (EitherT f) (EitherT mea) = EitherT $ (<*>) <$> f <*> mea

instance (Monad m) => Monad (EitherT e m) where 
    return = pure
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (>>=) (EitherT meea) f = EitherT $
        -- (>>=) :: m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
        meea >>= (\eea -> case eea of
                            (Left e) -> return (Left e)
                            (Right a) -> runEitherT (f a))

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT meea) = EitherT $ swapEither <$> meea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT meea) = meea >>= (\eea -> either f g eea)
