import Data.Char

-- 2.
f2 = filter isUpper

-- 3.
capitalize :: [Char] -> [Char]
capitalize str = (toUpper . head $ str) : (tail str)

-- 4.
-- this is stupid.

-- 5.
f5 = toUpper . head

-- 6. to late :'(
