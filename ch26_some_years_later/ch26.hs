{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa

instance (Applicative m) => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure x = MaybeT $ pure (Just x)

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    -- (MaybeT mab) <*> (MaybeT ma) = MaybeT $ mab <*> ma -- nope
    (MaybeT mab) <*> (MaybeT ma) = MaybeT $ (<*>) <$> mab <*> ma

-- \^ same as for Compose. why does this work? without the `(<*>) <$>` part I have
-- ```ghc
-- Expected: m (Maybe a -> Maybe b)
-- Actual: m (Maybe (a -> b))
-- ```
-- And, on reflection, mapping (<*>) into this is exactly what gives me the needed type, if I recall that (<*>) actually takes just one argument, like all other functions in Haskell, i.e.
-- (<*>) :: Maybe (a -> b) -> (Maybe a -> Maybe b)

instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT mMa) >>= aMmMb = MaybeT $
        do
            ma <- mMa
            case ma of
                Nothing -> pure Nothing
                Just a -> runMaybeT $ aMmMb a

-- MaybeT m (Maybe a) -- entferne das `MaybeT`
-- m (Maybe a) -- hebe die Funktion über das m, und über das a
-- -> m (Maybe (MaybeT m b)) -- entferne den MaybeT Konstruktur
-- -> m (Maybe (m (Maybe b))) -- das äußere Maybe muss weg. Wenn es ein `Just` ist, ist es einfach. Wenn es ein `Nothing` ist... kann ich vielleicht ein m bauen, das ein `pure Nothing` hat, und somit sind Dinge getauscht?
--
-- Alternativ:
-- MaybeT m (Maybe a) -- entferne das `MaybeT`
-- nimm das (Maybe a) aus dem m via (>>=) und erhalte...
-- erstmal ein (Maybe a), mit dem kann man jetzt unterschiedliche Dinge machen.
-- Nothing -> pure Nothing ? dann kriege ich glaub ich m (m Maybe b), und das >>= braucht den 2. m layer (über pure), um ihn direkt wieder einzustampfen.
-- Just -> damit kriege ich jetzt das a. Mit dem a kriege ich ein `MaybeT m b`. Daraus kriege ich ein `m (Maybe b)`. Also habe ich jetzt `m (m (Maybe b))`, das ich ja haben wollte.
--
-- Kann ich via join Logik irgendwie darüber nachdenken?
-- `MaybeT m a` heißt ich hab
-- m (Maybe a) ->
-- m (Maybe (m (Maybe a))) -- jetzt fmap über das äußere m
-- Nothing -> pure Nothing -> m (m (Maybe a))
-- bzw. m (Nothing) -> m (m (Maybe a)) -- die äußeren beiden müsste man noch joinen
-- Just x -> x -> m (m Maybe a)) -- die äußeren beiden müsste man noch joinen. hier ist das m schon im `x` enthalten.

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Functor m) => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance (Applicative m) => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure x = EitherT (pure (Right x))

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT me1) <*> (EitherT me2) = EitherT $ pure (<*>) <*> me1 <*> me2

instance (Monad m) => Monad (EitherT e m) where
    return = pure
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT me) >>= f = EitherT $ me >>= g
      where
        g (Left l) = pure $ Left l
        g (Right r) = runEitherT $ f r

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ fmap swapEither me

swapEither :: Either e a -> Either a e
swapEither (Left l) = Right l
swapEither (Right r) = Left r

eitherT :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT me) =
    me
        >>= ( \case
                (Left l) -> f l
                (Right r) -> g r
            )

-- Let's reduce this on either, as above?
eitherT' :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT' f g (EitherT me) = me >>= either' f g

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ g (Right r) = g r

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT rma) = ReaderT (fmap f . rma) -- thank you HLS, for this pointfree thingy I guess
    -- fmap f (ReaderT rma) = ReaderT $ \r -> f <$> rma r -- or like this
    -- fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma -- or like this, because ((->)e) implements Functor I guess

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure x = ReaderT $ const (pure x)

    -- pure = ReaderT . pure . pure -- or like this because ((->)e) implements Applicative I guess

    -- pure x = ReaderT $ \r -> pure x -- or like this

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (ReaderT rmab) <*> (ReaderT rma) =
        ReaderT $ \r ->
            let mab = rmab r
                ma = rma r
             in mab <*> ma

-- (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma -- or like this again...

-- ReaderT $ \r -> rmab r <*> rma r -- shorter alternative

instance (Monad m) => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= aRrmb = ReaderT $ \r ->
        let ma = rma r
         in ma
                >>= ( \a ->
                        -- rmb :: r -> m b
                        let rmb = (runReaderT . aRrmb) a in rmb r
                    )

-- or, equivalent, in do notation: (this is what the book does)
-- (ReaderT rma) >>= aRrmb = ReaderT $ \r -> do
--     a <- rma r
--     runReaderT (aRrmb a) r

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT smas) = StateT $ \s -> (\(a, s) -> (f a, s)) <$> smas s -- HLS tells me I could `Bifunctor.first` this. I guess...

-- TODO: why not Applicative?

instance (Monad m) => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> pure (a, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT smab) <*> (StateT sma) = StateT $ \s -> do
        (ab, s') <- smab s -- :: m (a -> b, s)
        (a, s'') <- sma s' -- :: m (a, s) -- actually, this could just be fmapped
        pure (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= asmb = StateT $ \s -> do
        (a, s') <- sma s
        (b, s'') <- runStateT (asmb a) s'
        pure (b, s'')

-- and now for MonadTrans

class MonadTrans t where
    -- | Lift a computation from the argument monad
    -- to the constructed monad.
    lift :: (Monad m) => m a -> t m a

instance MonadTrans MaybeT where
    lift :: (Monad m) => m a -> MaybeT m a
    lift ma = MaybeT $ Just <$> ma

-- lift ma = MaybeT $ pure <$> ma -- alternative without `Just`, but we know we have MaybeT, so why not use it
-- lift = MaybeT . fmap Just -- pointfree alternative

instance MonadTrans (ReaderT r) where
    lift :: (Monad m) => m a -> ReaderT r m a
    lift ma = ReaderT $ \r -> ma

-- lift ma = ReaderT $ const ma -- more pointfree alternative
-- lift = ReaderT . const       -- most pointfree alternative

-- Types
-- ma :: m a
-- ->
-- \r -> ma :: r -> m a
-- ReaderT $ \r -> m a :: ReaderT r m a

instance MonadTrans (EitherT e) where
    lift :: (Monad m) => m a -> EitherT e m a
    -- lift ma = EitherT $ fmap Right ma -- pointfull version
    lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
    lift :: (Monad m) => m a -> StateT s m a
    -- lift ma = StateT $ \s -> (,s) <$> ma -- alternative with TupleSections extension
    lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

class (Monad m) => MonadIO m where
    -- \| Lift a computation from the 'IO' monad.
    liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO :: IO a -> EitherT e m a
    liftIO ioa = EitherT $ liftIO (Right <$> ioa) -- this sorta reimplements lift from MonadTrans :O
    -- liftIO = lift . liftIO -- the solution, but... what?
    -- liftIO ioa = lift ((liftIO ioa)::Int)
    -- liftIO ioa :: m a
    -- lift (liftIO ioa) :: EitherT e m a
    -- liftIO ioa = EitherT $ Right <$> liftIO ioa -- alternative, this fmaps over one more layer!
    -- liftIO ioa = EitherT $ Right <$> ((liftIO ioa)::Int)
    -- liftIO ioa = Right <$> EitherT $ liftIO ioa -- this doesn't work, cannot apply EitherT on non-`Right`

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO :: IO a -> MaybeT m a
    -- liftIO = lift . liftIO -- entered without thinking. this works. why?
    liftIO ioa = MaybeT $ liftIO $ Just <$> ioa

-- liftIO ioa :: m a
-- lift (liftIO ioa) :: MaybeT m a

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO :: IO a -> ReaderT r m a
    -- liftIO = lift . liftIO -- srsly?
    -- liftIO ioa :: m a
    -- \r -> liftIO ioa :: r -> m a
    -- ReaderT $ ... :: ReaderT r m a
    liftIO ioa = ReaderT $ \r -> liftIO ioa

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO :: IO a -> StateT s m a
    -- liftIO = lift . liftIO -- I guess...
    liftIO ioa = StateT $ \s -> liftIO ((,s) <$> ioa)
