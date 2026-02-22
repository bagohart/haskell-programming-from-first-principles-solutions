{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

import GHC.Generics

-- import Test.QuickCheck.Gen (oneof) -- apparently not actually needed (any more?)?

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll
        charGen
        (\c -> ((charToMorse c) >>= morseToChar) == Just c)

-- main :: IO ()
-- main = quickCheck prop_thereAndBackAgain

-- Let's try stupid things for a while...

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = pure Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

main :: IO ()
main = do
    sample trivialGen

data Identity a = Identity a deriving (Eq, Show)

identityGen :: (Arbitrary a) => Gen (Identity a)
identityGen = do
    a <- arbitrary
    pure $ Identity a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary :: Gen (Identity a)
    arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenDouble :: Gen (Identity Double)
identityGenDouble = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    pure $ Pair a b

-- pairGen = Pair <$> arbitrary <*> arbitrary -- actually it's just this

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

-- oneof $ pure <$> [First a, Second b] -- actually...

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
    a <- arbitrary
    b <- arbitrary
    frequency $ [(10, pure (First a)), (1, pure (Second b))]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary
