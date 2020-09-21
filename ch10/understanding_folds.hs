-- 1. a is invalid, b and c are identical since * is commutative and associative
-- 2.
--      foldl (flip (*)) 1 [1..3]
-- =    foldl ((flip (*)) 1 1) [2..3]
-- =    foldl ((flip (*)) ((flip (*)) 1 1) 2) [3]
-- = ...
-- 3. c
-- 4. a
-- 5. 
import Data.Char

a = foldr (++) ["woot", "WOOT", "woot"] []
b = foldr max (chr 0) "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) False [False, True] -- ...
e = foldl (flip ((++) . show)) "" [1..5]
-- or:
-- e = foldl (\ys x -> ys ++ show x) "" [1..5]
f = foldr (const . show) "a" [1..5] -- hm. this one?
g = foldr const '0' "tacos"
h = foldl (flip const) '0' "burritos"
i = foldl (flip (const . show)) "z" [1..5] -- lol whatever.
