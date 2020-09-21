import Data.Tuple
import Data.List

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

-- this could probably be done in a more elegant way, but it is not an unfoldr because of the termination condition.
-- so digits 0 would be [] =/
digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise =  digits (n `div` 10) ++ [n `mod` 10]

-- hm...
-- actually, it is, but I need to catch the 0
-- which is not as nice, but anyway...
digits' 0 = [0]
digits' n = reverse . unfoldr (\n -> if n == 0 then Nothing else let (x,y) = divMod n 10 in Just (y,x)) $ n

-- this looks ok.
wordNumber :: Int -> String
wordNumber  = concat . intersperse "-" . map digitToWord . digits'
