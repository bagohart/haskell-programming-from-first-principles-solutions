{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
-- import Web.Scotty -- conflicts with the Trans thing above.

data Config = Config {
    counts :: IORef (M.Map Text Integer)
    , prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (newMap, newVal)
    where newVal = 1 + M.findWithDefault 0 k m -- or fromMaybe 0 $ lookup k m
          newMap = M.insert k newVal m

app :: Scotty ()
app = get "/:key" $ do
    unprefixed <- param "key"
    prefixx <- lift $ asks prefix -- there is a magic implicit ReaderT here, which we can use with even more magic :')
    let key :: Text
        key = unprefixed
        key' = mappend prefixx unprefixed 
    countoor <- lift $ asks counts
    liftIO $ modifyIORef' countoor (fst . (bumpBoomp key'))
    newInteger <- liftIO (M.lookup key' <$> readIORef countoor)
    html $ mconcat [ "<h1>Success! Count was: "
                    , TL.pack $ show newInteger
                    , "</h1>"
                    ]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then (error "Pass exactly one argument, please.") else return ()
    putStrLn ("Arguments: " ++ show args)
    counter <- newIORef M.empty
    let config = Config counter (TL.pack $ head args)
        -- runR :: Int -- use this to find out type of runR, or just look veeeeeery closely at the types
        runR m = runReaderT m config -- this design is probably what was actually expected in the exercise in ch. 22 o_O
    scottyT 3000 runR app

