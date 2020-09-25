data Optional a = Nada | Only a deriving (Eq, Show)

-- lol code from book won't work because superclass hierarchy has changed since o_O
instance Semigroup a => Semigroup (Optional a) where 
    (<>) Nada (Only x) = (Only x)
    (<>) (Only x) Nada = (Only x)
    (<>) (Only x) (Only y) = Only (x <> y)
    (<>) Nada Nada = Nada -- I could code golf this into the first two rules, but that doesn't make this any clearer.

instance Monoid a => Monoid (Optional a) where 
    mempty = Nada
