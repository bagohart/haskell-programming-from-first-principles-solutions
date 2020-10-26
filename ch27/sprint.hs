-- 1.
let x = 1
-- :sprint x
-- x = _
-- because x has no fixed type

-- 2.
let x = ['1']
-- x = ['1']
-- (or x = "1")
-- because data constructors and fixed type 

-- 3.
let x = [1]
-- x = _
-- inner type is not fixed. therefore, the whole thing is a function that awaits the num type,
-- so not even [_] is evaluated

-- 4.
let x = 1 :: Int
-- x = 1
-- fixed type, data constructor.

-- 5.
let f = \x -> x
let x = f 1
-- x = _
-- x is the result of function application, has not been computed yet
-- but no fixed type for 1
-- so x will not be remembered

-- 6.
let f :: Int -> Int; f = \x -> x
let x = f 1
-- x = _
-- fixed type for x because of type inference (f has concrete type here)
-- BUT
-- x is the result of function application, so it has not been computed yet.
-- type 'x' in ghci to compute it, and on the next try, :sprint x will yield
-- x = 1
