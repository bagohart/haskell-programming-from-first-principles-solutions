{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.IO.Class

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

instance Functor Identity where 
    fmap f (Identity x) = Identity $ f x

-- Identity :k: * -> *
-- id :: a -> a

instance Applicative Identity where 
    pure = Identity
    (<*>) :: (Identity (a -> b)) -> (Identity a) -> Identity b
    (<*>) (Identity ab) (Identity a) = Identity (ab a)

instance Monad Identity where 
    return = pure
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (>>=) (Identity a) f = f a

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where 
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT ma) = IdentityT $ f <$> ma

instance (Applicative m) => Applicative (IdentityT m) where 
    pure :: a -> IdentityT m a
    pure = IdentityT . pure
    (<*>) :: (IdentityT m (a -> b)) -> (IdentityT m a) -> (IdentityT m b)
    (<*>) (IdentityT mab) (IdentityT ma) = IdentityT $ mab <*> ma

instance (Monad m) => Monad (IdentityT m) where 
    return = pure
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    -- (>>=) (IdentityT ma) f = IdentityT $ join $ (runIdentityT . f) <$> ma
    -- and now the exciting part:
    --      ma # fmap f
    -- ~>   m (IdentityT m b) # fmap runIdentityT
    -- ~>   m (m b) # join
    -- ~>   m b # ...
    -- ~>   IdentityT m b
    -- ...
    -- on reflection, I just reinvented bind. so this could also be written as
    -- (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f
    -- note that here the runIdentityT must be glued onto the f, otherwise the join cannot work
    -- the book has this neat trick about stating type a to have the compiler tell me what it is:
    (>>=) (IdentityT ma) f =
        let 
            aimb = join $ runIdentityT <$> (f <$> ma)
            -- aimb :: a
         in IdentityT aimb

-- also
-- m >>= k =     IdentityT $ runIdentityT . k =<< runIdentityT m
-- ^ this just didn't use pattern matching to extract the ma, and it reversed the arguments.
-- not too interesting imo. also not more readable I think, because it's less obvious what's what. E.g. why is this thing
-- just called m and k here ,,?_?,,

------ previous stuff from ch. 25
------ new stuff now
instance (MonadIO m) => MonadIO (IdentityT m) where 
    liftIO = IdentityT . liftIO
