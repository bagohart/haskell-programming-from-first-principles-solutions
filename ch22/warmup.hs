import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    x <- cap
    y <- rev
    return (x,y)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (\x -> rev >>= (\y -> return (x,y)))
