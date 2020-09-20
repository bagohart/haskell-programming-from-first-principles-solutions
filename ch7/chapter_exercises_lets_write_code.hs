-- 1.
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
    where xLast = fst . (flip divMod) 10 $ x
          d     = snd . (flip divMod) 10 $ xLast

-- this exercise is a bit dumb, but hey...
tensDigit3 = snd . (flip divMod) 10 . fst . (flip divMod) 10

-- lol.
hunsD = tensDigit3 . (`div` 10)

-- 2.
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b = case b of
                    True -> y
                    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b | b = y
                | otherwise = x

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)

