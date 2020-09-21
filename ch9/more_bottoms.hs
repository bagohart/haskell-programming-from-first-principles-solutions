import Data.Bool

-- 1. bottom
-- 2. 2
-- 3. 2 : bottom
-- 4. maps for every character to bool if it is a vowel
itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- 5. a) [1^2, 2^2... 10^2]
--    b) [1,10,20]
--    c) [15,15,15]
-- 6. 
e7 = map (\x -> bool x (-x) (x == 3)) [1..10]
