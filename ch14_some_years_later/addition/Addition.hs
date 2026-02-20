module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

recMult :: (Eq a, Num a) => a -> a -> a
recMult x y = signum x * signum y * go (abs x) (abs y) 0
  where
    go 0 _ prod = prod
    go x' y' prod = go (x' - 1) y' (prod + y')

trivialInt :: Gen Int
trivialInt = pure 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1 :: Int) `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` (4 :: Int)
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0 :: Int)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2 :: Int)
        it "1 * 1 is 1" $ do
            recMult 1 1 `shouldBe` (1 :: Int)
        it "4 * 5 is 20" $ do
            recMult 4 5 `shouldBe` (20 :: Int)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionBroken :: Int -> Bool
prop_additionBroken x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

runQc2 :: IO ()
runQc2 = quickCheck prop_additionBroken

-- als nächstes: Morce Code Beispiel.
-- Guck das mal durch, da geht es auch darum, wie tests in stack abgebildet werden.
