import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
              ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\d ds -> case d of
                                 DbDate x -> x : ds
                                 _ -> ds
                     ) []

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\d ds -> case d of
                                 DbNumber x -> x : ds
                                 _ -> ds
                     ) []

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb db = let numbers = filterDbNumber db in
               fromIntegral (sum numbers) / fromIntegral (length numbers)
