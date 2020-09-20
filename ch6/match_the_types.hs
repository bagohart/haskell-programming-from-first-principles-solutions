import Data.List

-- 1.
i :: Num a => a
-- i :: a -- this does not work, but I'm not sure why this would be a problem in practice,
-- since it would mean that the value could be used in LESS cases.
-- So it seems that the type declaration can only add and not remove information.
i = 1

-- 2.
f :: Float
-- f :: Num a => a -- same case as above: literal requires a typeclass that is not required by a in this definition.
f = 1.0

-- 3.
-- f2 :: Float
f2 :: Fractional a => a -- this should work, see the error message from last task
f2 = 1.0

-- 4.
-- f3 :: Float
f3 :: RealFrac a => a -- this works, because RealFrac requires Fractional, as does 1.0, so this is a stricter type
-- adds more information?
f3 = 1.0

-- 5.
-- freud :: a -> a
freud :: Ord a => a -> a -- I don't get the reference, but this works since this is a stricter type than required
freud x = x
-- wait, I get it. This is about types. lmao.

-- 6.
freud' :: a -> a
-- freud' :: a -> Int -- this cannot work because of the implementation that requires that a is Int, so the type is too general.
freud' x = x

-- 7.
myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a -- this cannot work, is like last example
sigmund x = myX 

-- 8.
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a -- since the result is Int, a is Int, therefore this is too general
sigmund' x = myX

-- 9.
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int -- why not.
jung xs = head (sort xs)

-- 10.
-- young :: [Char] -> char
young :: Ord a => [a] -> a -- this is the signature that worked above with identical definition, so it must work...
young xs = head (sort xs)

-- 11.
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a -- this can't work, since using mySort implies a is Char
signifier xs = head (mySort xs)
