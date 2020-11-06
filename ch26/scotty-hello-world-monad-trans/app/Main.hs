{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)

import Control.Monad.Trans.Class

-- let's reimplement the stuff
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad -- for liftM aka fmap
import Control.Monad.Trans.State.Strict hiding (get) -- book says it should be lazy, but code says it is strict. hm.

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

-- main :: IO ()
main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        let hello = putStrLn "hello"
        -- concretizing the types for lift:
        -- (lift :: (Monad m, MonadTrans t) => m a -> t m a) hello
        -- (lift :: (MonadTrans t) => IO a -> t IO a) hello
        -- (lift :: IO a -> ActionM a) hello
        -- (lift :: IO () -> ActionM ()) hello

        -- concretizing the implementation for lift:
        -- (ActionT . lift . lift . lift) (putStrLn "hello")
        -- (ActionT . (ExceptT . liftM Right) . (liftReaderT) . lift) (putStrLn "hello")
        -- (ActionT . (ExceptT . liftM Right) . (ReaderT . const) . lift) (putStrLn "hello")
        (ActionT . (ExceptT . liftM Right) . (ReaderT . const) . (\m -> StateT (\s -> do
                a <- m
                return (a, s))
                                                                     )) (putStrLn "hello")
        -- (lift :: IO a -> ActionM a) hello
        -- lift (putStrLn "hello") -- dafuq, this is new
        -- lift (putStrLn beam) -- wrong text type
        html (mconcat [ "<h1>Scotty, ", beam, " me up!</h1>"])

-- type ScottyM = ScottyT Text IO
-- type ActionM = ActionT Text IO
