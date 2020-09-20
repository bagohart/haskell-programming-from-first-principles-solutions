-- 1. all of them are equivalent
-- 2. it's d. type of 3 is Num.
mTh x y z = x * y * z

-- 3.
addOneIfOdd n = case odd n of
                  True -> f n
                  False -> n
                  where f = \n -> n + 1

addFive = \x y -> (if x > y then y else x) + 5

mflip f x y = f y x

