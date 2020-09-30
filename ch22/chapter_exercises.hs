{-# LANGUAGE InstanceSigs #-}

module ReaderPractice where

import Control.Applicative
import Data.Maybe

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

--------------------------------------------------
    
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]
    
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ x `zip` y

ys :: Maybe Integer
ys = lookup 6 $ y `zip` z

-- this will be nothing
zs :: Maybe Integer
zs = lookup 4 $ x `zip` y

z' :: Integer -> Maybe Integer
z' n = lookup n $ x `zip` z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'
-- which is equivalent to
-- x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequA 7
    print $ "now new stuff"
    print $ foldr (&&) True (sequA 6)
    print $ sequA <$> (fromMaybe undefined) $ s'
    -- or just:
    -- print $ sequA (fromMaybe undefined s')
    print $ bolt <$> (fromMaybe undefined) $ ys
    -- the previous two seem boring. what is this about?
