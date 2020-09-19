-- 1.
x = (+)
f xs = w `x` 1
    where w = length xs

-- 2.
expr2 = \ x -> x

-- 3.
expr3 = \ (x:xs) -> x

-- 4.
f' (a, b) = a
