import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

maybeUnwrap :: (ExceptT String
                    (ReaderT () IO))
                    (Maybe Int)
maybeUnwrap = runMaybeT embedded                

eitherUnwrap :: ReaderT () IO
                    (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap                    

readerUnwrap :: ()
             -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

embedded :: MaybeT
                (ExceptT String
                    (ReaderT () IO))
                Int
embedded = return 1                    

embedded'' :: MaybeT
                (ExceptT String
                    (ReaderT () IO))
                Int
embedded'' = MaybeT $ ExceptT $ ReaderT $ return <$> (const (Right (Just 1)))

embeddedUnwrap2 = runReaderT $ runExceptT $ runMaybeT embedded''

embedded' :: MaybeT
                (ExceptT String
                    (ReaderT () IO))
                Int
embedded' = MaybeT $ v1
    where v1 :: ExceptT String (ReaderT () IO) (Maybe Int)
          v1 = ExceptT $ v2
          v2 :: ReaderT () IO (Either String (Maybe Int))
          v2 = ReaderT $ v3
          v3 :: () -> IO (Either String (Maybe Int))
          v3 = return <$> (const (Right (Just 1)))
          -- v3 = const $ return $ Right (Just 1)
          -- v3 = \_ -> return $ const (Right (Just 1))
          -- v3 = undefined
