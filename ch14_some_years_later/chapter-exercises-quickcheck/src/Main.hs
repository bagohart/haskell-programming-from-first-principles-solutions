{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List (sort)
import Test.QuickCheck -- needed for running quickQueck in ghci

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
