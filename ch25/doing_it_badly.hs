{-# LANGUAGE InstanceSigs #-}
import Control.Monad

-- let's build this one-off type which the book doesn't want me to build.

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where 
    fmap f (MaybeIO ioma) = MaybeIO $ (fmap . fmap) f ioma

instance Applicative MaybeIO where 
    pure x = MaybeIO $ pure (pure x)
    (<*>) (MaybeIO iomab) (MaybeIO ioma) = MaybeIO $ (<*>) <$> iomab <*> ioma

-- the last two are easy, they were just instances of Compose

instance Monad MaybeIO where 
    return = pure
    (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    (>>=) (MaybeIO ioma) f = op ((fmap . fmap) f ioma)
        where op :: IO (Maybe (MaybeIO b)) -> MaybeIO b
              op x = MaybeIO $ join $ fmap lol x
              lol :: (Maybe (MaybeIO b)) -> IO (Maybe b)
              lol Nothing = pure Nothing
              lol (Just iomb) = runMaybeIO iomb
              lol' :: (Maybe (MaybeIO b)) -> IO (Maybe b)
              lol' x = fmap join (sequenceA (fmap runMaybeIO x))

-- IO (Maybe a) -> IO (Maybe (IO (Maybe b)))

-- IO (Maybe a) ~> IO (Maybe (IO (Maybe b))) ~> ... I _could_ implement this by casing on the Maybe.
-- Is this a good idea? hm. Let's see.
-- IO (Maybe a) ~> IO (Maybe (IO (Maybe b))) ~> ... (case this)
-- = IO (Just (IO (Maybe b))) ~> IO (IO (Maybe b)) ~> IO (Maybe b)
-- = IO (Nothing) ~> IO (IO (Nothing)) ~> IO (Nothing) ~> IO (Maybe b)
--
-- this works. y-yaaaayyy... hm.
-- ... how? I basically reimplemented Maybe (lol), but then refactored it to use the Traversable thing of Maybe (lol').
-- and a lot of Functormagic. 
-- the concrete thing here was in fact only the use of 'runMaybeIO' to get some value.  (Traversable is not a superclass of Monad, so this is sort of concrete, too.)
-- hm.
