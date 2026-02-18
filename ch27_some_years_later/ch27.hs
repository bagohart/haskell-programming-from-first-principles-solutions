hypo' :: IO ()
hypo' = do
    let x :: Integer
        x = undefined
    s <- getLine
    case x `seq` s of
        "hi" -> print x
        _ -> putStrLn "hello"

notGonnaHappenBru :: Int
notGonnaHappenBru =
    let x = undefined
        y = 2
        z = (x `seq` y `seq` 10, 11)
     in snd z

data Test = A Test2 | B Test2 deriving (Show)
data Test2 = C Int | D Int deriving (Show)

force1 :: Test -> Int
force1 _ = 0

-- > force1 undefined
-- > 0

force2 :: Test -> Int
force2 (A _) = 1
force2 (B _) = 2

-- force2 (A undefined)
-- > 1
-- force2 (B undefined)
-- > 2
-- force2 undefined
-- > undefined

force3 :: Test -> Int
force3 (A (C i)) = i
force3 (B (C i)) = i
force3 (A (D i)) = i
force3 (B (D i)) = i

-- force3 (A (C undefined))
-- > undefined
-- force3 (A undefined)
-- > undefined
-- force3 undefined
-- > undefined

force4 :: Test -> Int
force4 (A (C _)) = 1
force4 (B (C _)) = 2
force4 (A (D _)) = 3
force4 (B (D _)) = 4

-- force4 A (C undefined)
-- > 1
-- force4 A (undefined)
-- > undefined
-- force4 undefined
-- > undefined

hypo'' :: IO ()
hypo'' = do
    let x :: Integer
        x = undefined
    s <- x `seq` getLine
    case s of
        "hi" -> print x
        _ -> putStrLn "hello"

-- continue: bei "using trace to observe sharing" hab ich aufgehört.
-- außerdem TODO: MonadTrans instanzen und MonadIO hab ich noch nicht komplett ankifiziert.
