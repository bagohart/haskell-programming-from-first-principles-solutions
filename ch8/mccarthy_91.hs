mccarthy :: Integer -> Integer
mccarthy n 
  | n > 100 = n - 10
  | otherwise = mccarthy . mccarthy $ n + 11
