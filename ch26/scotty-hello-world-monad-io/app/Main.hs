{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)

import Control.Monad.IO.Class

main :: IO ()
main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        liftIO (putStrLn "hello")
        html (mconcat [ "<h1>Scotty, ", beam, " me up!</h1>"])

-- type ScottyM = ScottyT Text IO
-- type ActionM = ActionT Text IO
