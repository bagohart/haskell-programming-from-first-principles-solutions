newtype  Constant a b =
    Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
    fmap _ (Constant c) = (Constant c)
    -- o_O cannot write it like this: because then the phantom type doesn't change >_<
    -- fmap _ c = c

instance Monoid a => Applicative (Constant a) where 
    pure _ = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant (x <> y)
