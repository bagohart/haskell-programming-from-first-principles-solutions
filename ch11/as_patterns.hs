import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs'@(x:xs) (y:ys) = if x == y then isSubseqOf xs ys else isSubseqOf xs' ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map doubleThis . words
    where doubleThis w'@(w:ws) = (w', toUpper w : ws)
