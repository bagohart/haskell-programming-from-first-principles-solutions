myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = map (uncurry f) (myZip xs ys)

myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,)
