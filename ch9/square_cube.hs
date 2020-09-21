-- this task is a bit imprecise. let's guess different ways of what it is getting at o_O

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1.
expr1 = zip mySqr myCube

-- 2.
expr2 = zip (filter (<50) mySqr) (filter (<50) myCube)

-- 3.
expr3 = length expr2

-- and now with list comprehensions. this is probably what was intended.
e1 = [(a,b) | a <- mySqr, b <- myCube]

e2 = [(a,b) | a <- mySqr, a < 50, b <- myCube, b < 50]

e3 = length e2
