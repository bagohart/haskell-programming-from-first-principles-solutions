data DividedResult = Result Integer | DividedByZero deriving Show

-- not sure about the task, but probably something like this is asked for, but in less ugly.
dividedBy :: Integer -> Integer -> (DividedResult, DividedResult)
dividedBy num denom = if denom == 0 then (DividedByZero, DividedByZero) else (fixResult (go (abs num) (abs denom) 0))
    where   go n d count
                | n < d = (count, n)
                | otherwise = go (n -d) d (count + 1)
            fixResult (x,y)
              | signum (num * denom) == 1 = (Result x, Result y)
              | signum (num * denom) == -1 && y == 0 = (Result $ (-x), Result 0)
              | signum (num * denom) == -1 && y /= 0 = (Result $ (-x) - 1, Result $ denom - (abs ((x+1)*y) - (abs num)))
