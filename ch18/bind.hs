import Control.Monad (join)

-- join :: Monad m => m (m a) -> m a

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . (fmap f)
-- or:
-- bind f moo = join $ f <$> moo

e = bind (\x -> [x, 1]) [2,3,4]

