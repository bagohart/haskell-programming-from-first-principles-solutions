-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f = (==) . f
-- or like this, which is maybe more readable, depending on your amount of stockholm syndrome
-- chk f a b = f a == b

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = foldr (+) 0 $ replicate (fromIntegral i) (f a)
-- or like this, which is not really nicer xD
-- arith f i = (foldr (+) 0) . (replicate (fromIntegral i)) . f
