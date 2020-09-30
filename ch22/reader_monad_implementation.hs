{-# LANGUAGE InstanceSigs #-}

-- motivation
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- if it's two distinct values:
froot' :: (Functor t, Num a, Foldable f) => t a -> f b -> (t a, Int)
froot' ta fb = (fmap (+1) ta, length fb)

-- what the book actually meant o_O
froot :: Num a => [a] -> ([a], Int)
froot xs = (fmap (+1) xs, length xs)

-- or actually more general?
froot2 :: (Num a, Foldable f, Functor f) => f a -> (f a, Int)
froot2 t = (fmap (+1) t, length t)

-- reusing the things
-- the book calls something similar 'frooty'
froot3 :: (Num a, Foldable f, Functor f) => f a -> (f a, Int)
froot3 t = bar (foo t) t

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

bindAllTheThings :: (a -> b) -> (b -> a -> c) -> a -> c
bindAllTheThings m k = \r -> k (m r) r

-- make it more similar to Reader by using r:
bindAllTheThings' :: (r -> a) -> (a -> r -> b) -> r -> b
bindAllTheThings' m k = \r -> k (m r) r

---------------------------------------------------------------------------------------
-- actual thing here
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where 
    fmap f (Reader r) = Reader $ f . r


instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)


instance Monad (Reader r) where 
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r


-- apply this instance
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' = runReader $ do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy

getDogRM'' :: Person -> Dog
getDogRM'' = runReader $ (Reader dogName) >>=
    (\name -> (Reader address >>=
        (\addy -> return $ Dog name addy)))
    
pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")
