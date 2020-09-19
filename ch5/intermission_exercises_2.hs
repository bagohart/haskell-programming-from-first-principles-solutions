f :: a -> a -> a -> a
f = undefined

x :: Char
x = undefined

-- 1.
-- x :: Char
-- f x :: Char -> Char -> Char

-- 2.
g :: a -> b -> c -> b
g = undefined
-- g 0 'c' "woot" :: Char

-- 3.
h :: (Num a, Num b) => a -> b -> b
h = undefined
-- h 1.0 2 :: Num b => b

-- 4.
h2 :: (Num a, Num b) => a -> b -> b
h2 = undefined
-- h2 1 (5.5 :: Double) :: Double

-- 5.
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
-- jackal "keyboard" "has ..." :: String

-- 6.
jackal2 :: (Ord a, Eq b) => a -> b -> a
jackal2 = undefined
-- jackal2 "keyboard" :: (Eq b) => b -> String

-- 7.
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
-- kessel 1 2 :: (Ord a, Num a) => a
-- numbers do not always implement Ord!

-- 8.
kessel2 :: (Ord a, Num b) => a -> b -> a
kessel2 = undefined
-- kessel2 1 (2 :: Integer) :: (Ord a, Num a) => a

-- 9.
kessel3 :: (Ord a, Num b) => a -> b -> a
kessel3 = undefined
-- kessel3 (1 :: Integer) 2 :: Integer
