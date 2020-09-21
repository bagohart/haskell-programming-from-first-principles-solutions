-- 1.
-- dividedBy 15 2
-- = go 15 2 0
-- = go (15-2) 2 (0+1)
-- = go ((15-2)-2) 2 ((0+1)+1)
-- = ...
-- = go 15-2-2-2-2-2...-2 2 0+1+1....+1
-- = (0+1+1...+1, 15-2-2...-2)

-- 2.
recSum :: (Eq a, Num a) => a -> a
recSum n = go n 0
    where go 0 sum = sum
          go n sum = go (n-1) (sum + n)

-- 3.
recMult :: (Integral a) => a -> a -> a
recMult x y = signum x * signum y * go (abs x) (abs y) 0
    where go 0 y prod = prod
          go x y prod = go (x-1) y (prod + y)
