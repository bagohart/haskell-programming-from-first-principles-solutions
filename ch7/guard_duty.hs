avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.9 = 'F'
  | otherwise = 'Z'
  where y = x / 100

-- 3. b
-- 4. the type reverse accepts, i.e. [a]
-- 5. pal :: [a] -> Bool
-- 6. c
-- 7. Num + Ord (or does the 0 in the comparison default to Integer? (Nope))
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
-- 8. numbers :: (Num a, Ord a, Num p) => a -> p
