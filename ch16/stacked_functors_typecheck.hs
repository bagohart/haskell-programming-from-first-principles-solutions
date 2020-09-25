-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor f => (x -> y) -> f x -> f y
--
--
-- fmap :: Functor f => (m -> n) -> f m -> f n
--                          b    ->    c      
-- fmap :: Functor f => (x -> y) -> f x -> f y
--                          a          b

-- (fmap . fmap) :: a        ->      c
-- (fmap . fmap) :: (x -> y) -> (f m -> f y)
--
-- and since b = (m -> n) = (f x -> f y) we have that m = f x and n = f y, so

-- (fmap . fmap) :: (x -> y) -> (f (f x) -> f (f y))
-- which is
-- (fmap . fmap) :: (x -> y) ->  f (f x) -> f (f y) 
--
-- BAM
