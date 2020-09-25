-- 1.
a = fmap (+1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])


-- 3.
c = (*2) . (\x -> x - 2)

-- 4.
-- dafuq. the return '1' is meant to confuse me. lol. lists are monads, but we haven't discussed that yet.
d = ((return  '1' ++) . show) . (\x -> [x, 1..3])

-- -- 5.
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed

-- 5.
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

