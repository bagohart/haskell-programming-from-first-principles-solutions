import Control.Applicative (liftA3)

stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos l1 l2 l3 = (,,) <$> l1 <*> l2 <*> l3

-- or with liftA3
combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' l1 l2 l3 = liftA3 (,,) l1 l2 l3
