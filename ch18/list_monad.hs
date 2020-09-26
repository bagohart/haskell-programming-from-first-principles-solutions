import Data.Bool

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = xs >>= (\x -> bool [] [x*x, x*x] (even x))

multItself :: [Int] -> [Int]
multItself xs = xs >>= (\n -> take n (repeat n))

