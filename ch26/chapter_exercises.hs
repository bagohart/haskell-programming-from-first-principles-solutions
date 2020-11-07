import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 1.
rDec :: Num a => Reader a a
-- (Reader r) is actually ReaderT r Identity, so we need the ReaderT data constructor
-- and the return for embedding in Identity
rDec = ReaderT $ \r -> return $ (subtract 1 r)
-- or with Identity:
-- rDec = ReaderT $ \r -> Identity $ (subtract 1 r)

-- then, runReader  rDec 5 ~> 4
-- and   runReaderT rDec 5 ~> Identity 4

-- 2.
-- this looks less reader-y, but it works
rDec' :: Num a => Reader a a
rDec' = ReaderT $ return . (subtract 1)

-- 3.
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \r -> Identity $ (show r)

-- 4.
-- The complicated type signature
-- rShow' :: Show a => ReaderT a Identity String
-- is equivalent to
rShow' :: Show a => Reader a String
rShow' = ReaderT $ return . show

-- 5.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> putStrLn ("Hi: " ++ (show r)) >> return (r+1)

-- 6.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> putStrLn ("Hi: " ++ (show s)) >> return (show s, s+1)

-- can I use 1-4 to express 5 and 6?
-- The code is easy enough, and inc != dec, but... maybe? it looks a bit disconnected like it's now
