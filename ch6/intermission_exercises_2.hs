-- 1.
data TisAnInteger = Tisan Integer

instance Eq TisAnInteger where
    (==) (Tisan i) (Tisan j) = i == j

-- 2.
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i1 i2) (Two j1 j2) = i1 == j1 && i2 == j2

-- 3.
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt _) (TisAString _) = False
    (==) (TisAnInt i) (TisAnInt j) = i == j
    (==) (TisAString s) (TisAString t) = s == t

-- 4.
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x1 x2) (Pair y1 y2) = x1 == y1 && x2 == y2

-- 5.
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- 6.
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne _) (ThatOne _) = False
    (==) (ThatOne _) (ThisOne _) = False
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y

-- 7.
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
