data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where 
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

-- cannot apply f to (First a) since (Sum a) is the Functor, but something like (Sum * b) is not writable in Haskell.
-- Actually, why exactly would this be a bad idea?
-- ^ Flip f a b = Flip (f b a) is a thing. lol.

