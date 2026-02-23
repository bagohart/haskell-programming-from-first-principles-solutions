{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Char
import Data.List (sort)
import Test.QuickCheck -- needed for running quickQueck in ghci
import Test.QuickCheck.Function

-- I'm... not sure what the () does?

main :: IO ()
main = do
    quickCheck prop_halfIdentity
    quickCheck (prop_listOrdered :: [Int] -> Bool) -- is there a way to use a more general type here?
    quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (plusCommutative :: Int -> Int -> Bool)
    quickCheck (multAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (multCommutative :: Int -> Int -> Bool)
    quickCheck (quotRem1 :: Int -> NonZero Int -> Bool)
    quickCheck prop_quotRem1
    quickCheck prop_quotRem2
    quickCheck (powAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (powCommutative :: Int -> Int -> Bool)
    quickCheck (listReverseIsId :: [Int] -> Bool)
    quickCheck (dollarStillWorks :: Fun Int Int -> Int -> Bool)
    quickCheck (dotStillWorks :: Int -> Fun Int Int -> Fun Int Int -> Bool)
    quickCheck (isFoldrOk1 :: [Int] -> [Int] -> Bool)
    quickCheck (isFoldrOk2 :: [[Int]] -> Bool)
    quickCheck (listLength :: Int -> [Int] -> Bool)
    quickCheck (readShowRoundTrip :: Double -> Bool)
    quickCheck (squareIdentity :: NonNegative Double -> Bool)
    quickCheck testCapitalizeWord
    quickCheck (testSort :: [Int] -> Bool)

-- 1.

half x = x / 2

halfIdentity = (* 2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2.

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

-- 5.
-- ohh, these are more interesting now.
-- Division by zero breaks everything, so I need to pick the numbers manually!

-- ChatGPT suggests restricting the type instead of writing custom generators
-- I guess that works?
quotRem1 :: (Integral a) => a -> NonZero a -> Bool
quotRem1 x (NonZero y) = (quot x y) * y + (rem x y) == x

quotRem2 :: (Integral a) => a -> NonZero a -> Bool
quotRem2 x (NonZero y) = (div x y) * y + (mod x y) == x

-- hmm... I can write a generator to exclude zero, that's not hard
-- but then what? How do I feed this to quickCheck?
-- How was this not mentioned in the book? o_O
-- Ah wait, it does. It's the forAll thing that looks a bit like magic.

nonTerminatingNonZeroIntGen :: (Integral a, Arbitrary a) => Gen a
nonTerminatingNonZeroIntGen = do
    a <- arbitrary
    case a of
        0 -> nonTerminatingNonZeroIntGen -- I guess there's a better way than this, but this might work... actually it doesn't if there's no randomness and it always fetches the same one. lol.
        x -> pure x

-- TODO: try the same thing with a generator instead

-- chatGPT suggests this, maybe it works:
nonZeroGen :: (Integral a, Arbitrary a) => Gen a
nonZeroGen = suchThat arbitrary (/= 0)

prop_quotRem1 :: Property
prop_quotRem1 =
    forAll nonZeroGen $ \y ->
        forAll arbitrary $ \(x :: Int) ->
            quotRem1' x y

prop_quotRem2 :: Property
prop_quotRem2 =
    forAll nonZeroGen $ \y ->
        forAll arbitrary $ \(x :: Int) ->
            quotRem2' x y

quotRem1' :: (Integral a) => a -> a -> Bool
quotRem1' x y = (quot x y) * y + (rem x y) == x

quotRem2' :: (Integral a) => a -> a -> Bool
quotRem2' x y = (div x y) * y + (mod x y) == x

-- 6.
-- ... there's more to do here.
-- this task suffers a bit from me not really knowing what this library can do
-- in the face of almost-fully polymorphic a =/
powAssociative :: (Integral a) => a -> a -> a -> Bool
powAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

-- Found counter-example: 0,0,0

powCommutative :: (Integral a) => a -> a -> Bool
powCommutative x y = x ^ y == y ^ x

-- Found counter-example: 0,1

-- 7.
listReverseIsId :: (Eq a) => [a] -> Bool
listReverseIsId xs = (reverse . reverse) xs == xs

-- 8.

-- I could newtype functions like this:
-- newtype MyFunction a b = MyFunction { getMyFunction :: a -> b }
-- but how do I implement show s.t. the counter-exampled thingys get shown?
-- I guess I do need to use quickCheck functionality for this kind of thing?

dollarStillWorks :: (Eq b) => Fun a b -> a -> Bool
dollarStillWorks (Fun _ f) x = (f $ x) == f x

dotStillWorks :: (Eq c) => a -> Fun b c -> Fun a b -> Bool
dotStillWorks a (Fun _ f) (Fun _ g) = (f . g) a == (\x -> f (g x)) a

-- 9.
isFoldrOk1 :: (Eq a) => [a] -> [a] -> Bool
isFoldrOk1 xs ys = foldr (:) xs ys == (++) xs ys

-- Found counterexample: [0], [1]

isFoldrOk2 :: (Foldable t, Eq a) => t [a] -> Bool
isFoldrOk2 xs = foldr (++) [] xs == concat xs

-- 10.
listLength :: (Arbitrary a) => Int -> [a] -> Bool
listLength n xs = length (take n xs) == n

-- Found counterexample: 1, []

-- 11.
readShowRoundTrip :: (Eq a, Read a, Show a) => a -> Bool
readShowRoundTrip x = (read . show) x == x

square :: (Num a) => a -> a
square x = x * x

-- "Failure"

-- squareIdentity :: (Eq a, Floating a) => a -> Bool
-- This fails for -1.0.
-- I guess that's true, but that was not the interesting part...
-- Let's remove negative numbers then...
squareIdentity :: (Floating a, Eq a) => NonNegative a -> Bool
squareIdentity (NonNegative x) = (square . sqrt) x == x

-- Found counterexample: 0.2

-- "Idempotence"
twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

-- the original function in the task is broken :)
testCapitalizeWord :: String -> Bool
testCapitalizeWord x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (w : ws) = toUpper w : ws

testSort :: (Ord a) => [a] -> Bool
testSort x = (sort x == twice sort x) && (sort x == fourTimes sort x)

-- Make a Gen random generator for the datatype
data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

instance Arbitrary Fool where
    arbitrary :: Gen Fool
    arbitrary = genFool

data Fool' = Fulse' | Frue' deriving (Show, Eq)

genFool' :: Gen Fool'
genFool' = elements [Fulse', Fulse', Frue']
-- this should be equivalent, but is actually more noise:
-- genFool' = frequency [(2, pure Fulse'), (1, pure Frue')]
