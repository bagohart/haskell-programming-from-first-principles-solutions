fibs = 1 : scanl (+) 1 fibs

-- 1.
fibs1 = take 20 $ 1 : scanl (+) 1 fibs1

-- 2.
fibs2 = takeWhile (<100) $ 1 : scanl (+) 1 fibs2

-- 3.
factorial :: [Integer]
factorial = scanl (*) 1 [1,2..]

factorialN :: Int -> Integer
factorialN = (factorial !!)
