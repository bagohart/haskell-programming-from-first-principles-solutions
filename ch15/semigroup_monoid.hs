-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where 
    _ <> _ = Trivial

instance Monoid Trivial where 
    mempty = Trivial

-- 2.
newtype Identity a = Identity a
instance Semigroup a => Semigroup (Identity a) where 
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where 
    mempty = Identity mempty

-- 3.
data Two a b = Two a b
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty = Two mempty mempty

-- 4. 5. boring

-- 6.
newtype BoolConj = BoolConj Bool
instance Semigroup BoolConj where 
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where 
    mempty = BoolConj True

-- 7.
newtype BoolDisj = BoolDisj Bool
instance Semigroup BoolDisj where 
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where 
    mempty = BoolDisj False

-- 8.
data Or a b = Fst a | Snd b
instance Semigroup (Or a b) where 
    (Snd a) <> _ = Snd a
    _ <> b = b

-- 9.
newtype Combine a b = Combine { unCombine :: (a -> b) }
instance Semigroup b => Semigroup (Combine a b) where 
    (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance Monoid b => Monoid (Combine a b) where 
    mempty = Combine $ \n -> mempty

-- 10.
newtype Comp a = Comp { unComp :: (a -> a) }
instance Semigroup a => Semigroup (Comp a) where 
    (Comp f) <> (Comp g) = Comp $ f . g
    -- this could also be implemented like the previous thing

instance Monoid a => Monoid (Comp a) where 
    mempty = Comp id

-- 11.
data Validation a b = Failure a | Success b deriving (Eq, Show)

-- combine failures, keep first success, prefer success over failure
instance Semigroup a => Semigroup (Validation a b) where 
    (Failure a) <> (Failure b) = Failure $ a <> b
    (Failure _) <> (Success b) = Success b
    (Success a) <> _ = Success a

------ Monoid only. (But we need Semigroup, too, because the hierarchies have been updated.)
-- 8.
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where 
    (Mem f) <> (Mem g) = Mem $ \x -> let (af,sf) = f x; (agsf,sgsf) = g sf in (af <> agsf, sgsf)
    -- I could also not chain the functions and compute f x <> g x and drop one element:
    -- (Mem f) <> (Mem g) = Mem $ \x -> let (af,sf) = f x; (ag,sg) = g x in (af <> ag, sf)
    -- but that seems wrong :)
    -- let's think, what is required for this to be monoidal?
    -- for the semigroup associativity is sufficient, so if I always throw away earlier results and just take the latest, like this
    -- (Mem f) <> (Mem g) = Mem g
    -- then it IS associative
    -- but then x <> mempty = mempty for all x, which is wrong for non-trivial monoids.
    -- Therefore, throwing away the result is not an option, and we need to chain them.
    -- What about the order of function application?
    -- f <> g <> h, if in f <> g, f is applied before g, then:
    -- (f <> g) <> h ~> (g . f) <> h ~> h . (g . f) and
    -- f <> (g <> h) ~> (g <> h) . f = (h . g) . f
    -- Otherwise, if in f <> g, g is applied before f, then:
    -- (f <> g) <> h ~> (f . g) <> h ~> (f . g) . h and
    -- f <> (g <> h) ~> f <> (g . h) ~> f . (g . h)
    -- which seems, ok, too.
    -- Identities? Applying id first or last doesn't make any difference.
    -- What about the examples?
    -- What about switching the order of function application vs switching the order of <> application?
    -- (Mem f) <> (Mem g) = Mem $ \x -> let (ag,sg) = g x; (a2,s2) = f sg in (ag <> a2, s2)

-- not sure about all of this. I'd need a formal proof or actually do the quick check thing.
-- Let's try formal proof, i.e. program calculation?
-- Mem f <> mempty $ a = Mem $ \x -> (fst . f . id $ x <> mempty, snd . f . id $ x)
-- = Mem $ \x -> (fst . f $ x <> mempty, snd . f $ x)
-- = Mem $ \x -> (fst . f $ x, snd . f $ x)
-- = Mem f
--
-- (Mem f <> Mem g) <> Mem h =?= Mem f <> (Mem g <> Mem h)
-- (Mem f <> Mem g) <> Mem h = Mem $ \x -> (fst . f . fst . g $ x <> fst . f $ x, snd . f . snd . g $ x) <> Mem h
-- = Mem $ \x -> ... this is getting too hairy, let's try it a bit simpler with what should be the result:
--
-- Mem f <> Mem g means: the value is passed through g, then f, to arrive final value of the right
-- to arrive left value, it means f(g(x)) <> g(x)
-- adding h to that on the right side means:
-- f(g(h(x))) and f(g(h(x)) <> g(h(x)) <> h(x)
-- ... this is confusing.
-- But it seems the sane path is apply the functions from right to left, and do something like a scanr, but of differentfunctions, on x and then mappend all of them together.
-- todo: look at this later with fresh eyes.

instance Monoid a => Monoid (Mem s a) where 
    -- mempty = Mem $ \x -> (mempty, x)
    mempty = Mem $ (,) mempty . id

f' = Mem $ \s -> ("hi", s + 1)

main = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
