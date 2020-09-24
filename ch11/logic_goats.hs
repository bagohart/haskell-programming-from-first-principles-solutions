{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany = (>42)

instance TooMany String where
    tooMany = (>5) . length

instance TooMany (Int, String) where
    tooMany (n, s) = tooMany n || tooMany s

instance TooMany (Int, Int) where
    tooMany (n,m) = n+m > 84

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n,m) = tooMany n && tooMany m -- what is this lol

