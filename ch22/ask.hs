newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader $ \a -> a

-- this is also a possible function :')
ask' :: Reader a a
ask' = Reader $ \a -> undefined
