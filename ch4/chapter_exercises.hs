-- 4.
expr4 = 6 `div` length [1,2,3]

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

-- 10.
f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f = \t1 t2 -> ((snd t1, snd t2), (fst t1, fst t2))
