-- 1.
stops = "pbtdkg"
vowels = "aeiou"

a = [[s1,v,s2] | s1 <- stops, v <- vowels, s2 <- stops]
b = [['p',v,s2] | v <- vowels, s2 <- stops]
-- c is boring

-- 2.
seekritFunc :: [Char] -> Int
seekritFunc x = div 
                    (sum (map length (words x)))
                    (length (words x))
                    -- this seems to compute average word length

-- 3.
seekritFunc' :: [Char] -> Double
seekritFunc' x = (/) 
                    (fromIntegral (sum (map length (words x))))
                    (fromIntegral (length (words x)))

-- rewriting functions using folds. this is mostly boring.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = concat . foldr (\x ys -> if p x then [x]:ys else []:ys) [] 
