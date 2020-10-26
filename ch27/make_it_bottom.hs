-- {-# LANGUAGE BangPatterns #-}

x = undefined
y = "blah"
main = do
    print (snd (x, x `seq` y))

-- not allowed :)
-- !x' = undefined
-- y' = "blah"
