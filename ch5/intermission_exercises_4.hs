-- 1.
myConcat x = x ++ " yo"
-- myConcat :: [Char] -> [Char]

-- 2.
myMult x = (x / 3) * 5
-- myMult :: (Fractional a) => a -> a

-- 3.
myTake x = take x "hey you"
-- myTake :: Int -> [Char]

-- 4.
myCom x = x > (length [1..10])
-- myCom :: Int -> Bool

-- 5.
myAlph x = x < 'z'
-- myAlph :: Char -> Bool

