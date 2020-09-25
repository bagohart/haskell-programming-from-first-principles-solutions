data Validation e a = Failure e | Success a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure e) = (Failure e)
    fmap f (Success a) = Success $ f a

-- This is different
instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Success f) (Success g) = Success $ f g
    (<*>) (Failure f) (Success _) = Failure f
    (<*>) (Success _) (Failure f) = Failure f
    (<*>) (Failure f) (Failure g) = Failure $ f <> g
