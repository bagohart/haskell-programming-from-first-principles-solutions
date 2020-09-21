eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt l u = if l > u then [] else l : eftInt (l+1) u

eftChar :: Char -> Char -> [Char]
eftChar c d = if c > d then [] else c : eftChar (succ c) d

