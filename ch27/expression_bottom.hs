e1 = snd (undefined, 1)
-- 1

x = undefined
e2 = let y = x `seq` 1 in snd (x, y)
-- undefined

e3 = length $ [1..5] ++ undefined
-- undefined

e4 = length $ [1..5] ++ [undefined]
-- 6

e5 = const 1 undefined
-- 1

e6 = const 1 (undefined `seq` 1)
-- 1

e7 = const undefined 1
-- undefined
