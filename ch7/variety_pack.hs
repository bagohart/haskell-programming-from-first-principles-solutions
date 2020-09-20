-- 1.
-- k :: (a,b) -> a
k (x,y) = x

-- k1 :: Num a => a
-- ghci tells me this is Integer, but it accepts this definition, too.
-- maybe this is because it evaluates the expression and then defaults to a concrete value?
-- k1 :: Float -- this works, too
k1 = k ((4-1), 10)

-- k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)
-- this returns 3

f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (a,_,c) (d,_,f) = ((a,d), (c,f))
