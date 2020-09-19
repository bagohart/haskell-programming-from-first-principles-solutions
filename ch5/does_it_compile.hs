-- 1.
bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10
-- ^ this statement does nat make any sense, so I could do whatever, e.g.
wahoo = bigNum ^ 10

-- 2.
x = print
y = print "woohoo!"
z = x "hello world"
-- works.

-- 3.
a = (+)
b = 5
c = a b 10 -- ftfy
d = a c 200 -- ...

-- 4.
a2 = 12 + b
b2 = 10000 * a2 -- ftfy
