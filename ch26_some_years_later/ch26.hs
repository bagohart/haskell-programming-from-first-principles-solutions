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
