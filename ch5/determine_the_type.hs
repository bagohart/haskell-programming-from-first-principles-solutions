a1 = (* 9) 6 -- :: (Num a) => a
b1 = head [(0,"doge"), (1, "kitteh")] -- :: (Num a) => (a, [Char])
c1 = head [(0::Integer, "doge"), (1, "kitteh")] -- :: (Integer, [Char])
d1 = if False then True else False -- :: Bool
e1 = length [1,2,3,4,5] -- :: Int
f1 = (length [1,2,3,4] > length "TACOCAT") -- :: Bool

x = 5
y = x + 5
w = y * 10
-- w :: (Integral a) => a
-- ^ nope, it actually has a concrete type, defaulting to Integer
-- to see this, check e.g.:
f :: Int -> Int
f = id
-- f w is not allowed!

x2 = 5
y2 = x2 + 5
z2 y2 = y2 * 10
-- z2 :: (Num a) => a -> a

x4 = 5
y4 = x4 + 5
f4 = 4 / y4
-- (/) demands Fractional a, so it would seem that
-- f4 :: (Fractional a) => a
-- but that is not a concrete value, so...?
-- So it defaults to Double.

x5 = "Julie"
y5 = " <3"
z5 = "Haskell"
f5 = x5 ++ y5 ++ z5
-- f5 :: [Char]
