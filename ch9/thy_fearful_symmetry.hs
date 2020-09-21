import Data.List

-- 1.
myWords :: [Char] -> [[Char]]
myWords = unfoldr (\str -> let str' = dropWhile (==' ') str in
                               if null str'
                                  then Nothing
                                  else Just ((takeWhile (/=' ') str', dropWhile (/=' ') str')))

-- the other two exercises are boring, just replace ' ' with '\n' and then a variable separator
