{-
1.
const 1 undefined
~> (\a -> (\b -> a)) 1 undefined
~> (\b -> 1) undefined
~> 1

2.
const undefined 1
~> (\a -> (\b -> a)) undefined 1
~> (\b -> undefined) 1
~> undefined

3.
flip const undefined 1
~> flip (\a -> (\b -> a)) undefined 1
~> (\a -> (\b -> a)) 1 undefined
~> 1

4..
flip const 1 undefined
...
~> undefined

5.
const undefined undefined
~> (\a -> (\b -> a)) undefined undefined
~> undefined

6.
foldr const 'z' ['a'..'e']
~> const 'a' (foldr const 'z' ['b'..'e'])
~> (\a -> (\b -> a)) 'a' (foldr const 'z' ['b'..'e'])
~> 'a'

7.
foldr (flip const) 'z' ['a'..'e']
~> (flip const) 'a' (foldr const 'z' ['b'..'e'])
~> const (foldr const 'z' ['b'..'e']) 'a'
~> foldr const 'z' ['b'..'e']
~> ...
~> foldr const 'z' []
~> 'z'

-}
