import Control.Applicative

-- 1.
j :: Monad m => m (m a) -> m a
j = flip (>>=) id

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2
-- technically not a method of Monad (as the task requires), so I should use liftM2

-- 4.
a :: Monad m => m a -> m (a-> b) -> m b
a = flip (<*>)

-- 5.
-- trivial solution:
-- meh xs f = fmap (\x -> [x]) (head (map f xs)) -- this solution is both partial and trivial :')
-- idea for non-trivial solution: 
-- 1. go from [a] to [m b]
-- 2. then use voodoo to go from [m b] to m [b] (see below)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = switchMe (map f xs)

-- Idea: use applicative to join these things together
-- [m a, m a, ma a]
-- m a -> m (\as a -> as ++ a) 
-- ma1' <*> ma2' <*> ... <*> pure []
switchMe :: Monad m => [m a] -> m [a]
switchMe xs = foldr (<*>) (pure []) fdu
    where fdu = map (\ma -> fmap (\a -> (\as -> a:as)) ma) xs
          -- this looks like I reinvented liftA2 or something. this is probably bad.

-- this seems to work, but also looks very complicated. maybe there is an easier way to do this?
-- [a]
-- [a1, a2 ...]
-- a1 -> m b1
-- a2 -> m (b1, a2) -> m (b1, m b2)
-- (since m b2 can be mapped to m [b1, b2])
-- -> m (m [b1, b2]) -> m [b1, b2] (join)
--
-- differently put:
-- [a1, a2 ...]
-- a1 -> m b1 -> m [b1]
-- a2 -> m [b1] -> m ([b1], a2) -> m ([b1], m b2) -> m (m [b1,b2]) -> m [b1,b2]
--
-- or with a left identity:
-- [a1, a2 ...]
-- return [] # fmap
-- -> m ([], a1) fmap, f
-- -> m ([], m b1) fmap.fmap one
-- -> m ([], m [b1]) fmap.fmap (++)
-- -> m (m [] ++ [b1])
-- =  m (m [b1]) join
-- = m [b1]
--
-- but this wouldn't be better than the previous approach.
-- what about explicit recursion?
-- [] -> m []
-- [a1, a2 ...]
-- a1 -> m b1
-- m b1 (*) m [b] # fmap
-- m (b1, m [b]) # fmap
-- m (m b1:[b]) # join
-- m b1:[b]
-- ... maybe?
meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] _ = return []
meh' (a:as) f = (f a) `op` (meh as f)

op :: Monad m => m b -> m [b] -> m [b]
op mb mlb = op2 $ fmap (\b1 -> (b1,mlb)) mb

op2 :: Monad m => m (b, m [b]) -> m [b]
op2 = j . op3

op3 :: Monad m => m (b, m [b]) -> m (m [b])
op3 = fmap op4

op4 :: Monad m => (b, m [b]) -> m [b]
op4 (b1, mlb) = op5 b1 mlb

op5 :: Monad m => b -> m [b] -> m [b]
op5 b1 mlb = fmap (op6 b1) mlb

op6 :: b -> [b] -> [b]
op6 = (:)

-- that... worked. but I'm not convinced that this was a better way (:
-- Give it another try:
meh'' :: Monad m => [a] -> (a -> m b) -> m [b]
meh'' xs f = foldr (\x ys -> liftA2 (++) (one <$> (f x)) ys) (return []) xs
-- hm.

-- 6.
-- this... is something I did as a subfunction of 5, but the task wants me to reuse 5 for this task.
-- this is a bit disturbing x_X it implies that 5 shoud have been MUCH easier. hm.
-- anyway, this is easy, if one recognizes that a is an m b already:
flipType :: (Monad m) => [m a] -> m [a]
flipType lma = meh' lma id

one :: a -> [a]
one x = [x]
