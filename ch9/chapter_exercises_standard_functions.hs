import Data.List

myOr :: [Bool] -> Bool
myOr = foldl' (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldl' (\ys x -> x == e || ys) False

myElem2 e = myAny (e==)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp = foldl1' (\ys x -> if comp ys x == GT then ys else x)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp = myMaximumBy (\x y -> inv $ comp x y) -- this could actually profit from the owl operator maybe.
    where inv EQ = EQ
          inv LT = GT
          inv GT = LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
