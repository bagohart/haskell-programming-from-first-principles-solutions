-- 1.
e1 = filter (\n -> n `mod` 3 == 0) [1..30]

-- 2.
e2 = length . filter (\n -> n `mod` 3 == 0) $ [1..30]

-- 3.
myFilter = filter (\w -> not $ elem w ["the", "a", "an"]) . words
-- or in pointfree style:
myFilter2 = filter (not . (flip elem) ["the", "a", "an"]) . words
