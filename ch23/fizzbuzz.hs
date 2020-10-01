import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise  = show n

-- main :: IO ()
-- main = mapM_ (putStrLn . fizzBuzz) [1..100]
-- or:
-- main = sequenceA (map (putStrLn . fizzBuzz) [1..100]) >> return () -- o_O

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

-- main :: IO ()
-- main = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]

-- exercise
main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 1 100

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = fizzBuzzList [to,to-1..from]
