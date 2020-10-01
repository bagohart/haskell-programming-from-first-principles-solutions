{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where 
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi sas) = Moi $ \s -> let (a, s') = sas s in (f a, s')

instance Applicative (Moi s) where 
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (<*>) (Moi sab) (Moi sa) = Moi $
        \s -> let (a, s') = sa s
                  (ab, s'') = sab s' in
                  (ab a, s'')
                  -- it would also compile if I throw away the second state
                  -- result and just return the first one
                  -- but that would seem useless

instance Monad (Moi s) where 
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (>>=) (Moi sa) aMsb = Moi $
        \s -> let (a,s') = sa s in (runMoi . aMsb $ a) s'
    -- this seems to be strictly more powerfu than the applicative, since here the second computation
    -- can depend on the value of a, i.e. the result of the first computation!

-- copied from previous exercise, now chapter exercises
-- 1.
get :: Moi s s 
get = Moi $ \s -> (s,s)

-- 2.
put :: s -> Moi s ()
put s = Moi $ \_ -> ((),s)

-- 3.
exec :: Moi s a -> s -> s
exec (Moi sa) s = let (a,s') = sa s in s'

-- 4.
eval :: Moi s a -> s -> a
eval (Moi sa) s = let (a,s') = sa s in a


-- 5.
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
