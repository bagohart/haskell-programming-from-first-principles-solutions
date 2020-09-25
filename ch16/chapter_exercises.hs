import GHC.Arr

-- 1. nope
-- 2. yes:
data BoolAndSomethingElse a = False' a | True' a
instance Functor BoolAndSomethingElse where 
    fmap f (False' x) = False' $ f x
    fmap f (True' x) = True' $ f x

-- 3. yes
data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where 
    fmap _ Falsish = Falsish
    fmap f (Truish x) = Truish $ f x

-- 4. dafuq
newtype Mu f = InF { outF :: f (Mu f) }

-- kind analysis:
-- f (Mu f) must be *, therefore f is * -> *
-- Mu f is *, therefore Mu is * -> *, but the first star is f, so actually Mu must be (* -> *) -> *
-- So the thing must be mapped over something of kind * -> *
-- What does that even mean o_O
-- anyway, functor cannot be written for this, since functor needs kind * -> * which is different from (* -> *)-> *

-- 5.
data D = D (Array Word Word) Int Int
-- Array :k: * -> * -> *, so Array Word Word :k: *
-- but anyway, this won't work since on the left side, D doesn't even have a type variable, so it has kind *


