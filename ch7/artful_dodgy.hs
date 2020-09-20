dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
-- dodgy 1 1 = 11
-- dodgy 2 2 = 22
-- dodgy 1 2 = 21
-- dodgy 2 1 = 12

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1
-- oneIsOne 1 = dodgy 1 1 = 11
-- oneIsOne 2 = dodgy 1 2 = 21
-- oneIsOne 3 = dodgy 1 3 = 31

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
-- oneIsTwo 1 = dodgy 1 2 = 21
-- oneIsTwo 2 = dodgy 2 2 = 22
-- oneIsTwo 3 = dodgy 3 2 = 23
