import Data.List

-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x2 :: Maybe Int
x2 = elemIndex 3 [1..5]

y2 :: Maybe Int
y2 = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed' :: Maybe Int
maxed' = max' <$> x2 <*> y2

-- 4.
xs = [1..3]
ys = [4..6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x3 <*> y3)
-- or:
-- summed = ((<$>) sum) $ (,) <$> x3 <*> y3

