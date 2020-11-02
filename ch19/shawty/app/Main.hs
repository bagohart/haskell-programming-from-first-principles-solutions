{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

import Control.Monad.Trans.Reader -- for ex. in ch. 22 (use ReaderT)

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']
-- use this to test the collision detection feature
-- alphaNum = ['A']

randomElement :: String -> IO Char
randomElement xs = do
    let maxIndex :: Int
        maxIndex = length xs - 1
    randomDigit <- SR.randomRIO (0, maxIndex) -- IO Int
    return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
    concat
    [ "<a href=\""
    , shorty
    , "\">Copy and paste your short URL</a>"
    ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
    TL.concat [ TL.pack (show resp)
              , " shorty is: "
              , TL.pack (linkShorty shawty)
              ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
    TL.concat
    [ uri
    , " wasn't a url,"
    , " did you forget http://?"
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
    TL.concat
    [ "<a href=\""
    , tbs, "\">"
    , tbs, "</a>"
    ]

shortyWontOverwrite :: TL.Text -> TL.Text
shortyWontOverwrite uri = TL.concat
    [
    "Shawty almost overwrote "
    , "<a href=\""
    , uri, "\">"
    , uri, "</a>"
    ]


wtf :: ReaderT Int Maybe Int
wtf = do
    ReaderT $ \x -> Just (x+1)
    ReaderT $ \x -> Just 5

-- app :: R.Connection -> ScottyM () -- ex. 22 ReaderT
app :: ReaderT R.Connection ScottyM ()
-- app rConn = do
-- app = ReaderT $ \rConn ->
app = do
    -- this works but I'm not sure if this is an improvement x_X
    ReaderT $ \rConn -> 
        -- save a uri
        get "/" $ do
            uri <- param "uri"
            let parsedUri :: Maybe URI
                parsedUri = parseURI (TL.unpack uri)
            case parsedUri of
                Just _ -> do
                    shawty <- liftIO shortyGen
                    let shorty = BC.pack shawty
                        uri' = encodeUtf8 (TL.toStrict uri)
                    -- first shawty exercise: prevent overwriting of short URIs
                    -- aka collision detection
                    oldShawty <- liftIO (getURI rConn shorty)
                    case oldShawty of
                      Right (Just bs) -> 
                          html (shortyWontOverwrite bsLol)
                            where bsLol :: TL.Text
                                  bsLol = TL.fromStrict (decodeUtf8 bs)
                      otherwise -> do
                        resp <- liftIO (saveURI rConn shorty uri')
                        html (shortyCreated resp shawty)
                Nothing -> text (shortyAintUri uri)
    ReaderT $ \rConn ->
        -- fetch a uri
        get "/:short" $ do
            short <- param "short"
            uri <- liftIO (getURI rConn short)
            case uri of
              Left reply -> text (TL.pack (show reply))
              Right mbBS -> case mbBS of
                              Nothing -> text "uri not found"
                              Just bs -> html (shortyFound tbs)
                                where tbs :: TL.Text
                                      tbs = TL.fromStrict (decodeUtf8 bs)

-- let's try to use ReaderT in a different way, i.e. only once:
-- saveUri :: ???
saveUri :: R.Connection -> ScottyM ()
saveUri rConn = get "/" $ do
            uri <- param "uri"
            let parsedUri :: Maybe URI
                parsedUri = parseURI (TL.unpack uri)
            case parsedUri of
                Just _ -> do
                    shawty <- liftIO shortyGen
                    let shorty = BC.pack shawty
                        uri' = encodeUtf8 (TL.toStrict uri)
                    -- first shawty exercise: prevent overwriting of short URIs
                    -- aka collision detection
                    oldShawty <- liftIO (getURI rConn shorty)
                    case oldShawty of
                      Right (Just bs) -> 
                          html (shortyWontOverwrite bsLol)
                            where bsLol :: TL.Text
                                  bsLol = TL.fromStrict (decodeUtf8 bs)
                      otherwise -> do
                        resp <- liftIO (saveURI rConn shorty uri')
                        html (shortyCreated resp shawty)
                Nothing -> text (shortyAintUri uri)

getUri :: R.Connection -> ScottyM ()
getUri rConn =        get "/:short" $ do
            short <- param "short"
            uri <- liftIO (getURI rConn short)
            case uri of
              Left reply -> text (TL.pack (show reply))
              Right mbBS -> case mbBS of
                              Nothing -> text "uri not found"
                              Just bs -> html (shortyFound tbs)
                                where tbs :: TL.Text
                                      tbs = TL.fromStrict (decodeUtf8 bs)

-- this works too. looks clearer that way.
app' :: ReaderT R.Connection ScottyM ()
app' = ReaderT $ \rConn -> do
    saveUri rConn
    getUri rConn

-- and we can also inline this. not sure why it didn't work before. What's up with this layouting thing o_O
app'' :: ReaderT R.Connection ScottyM ()
app'' = ReaderT $ \rConn -> do
    get "/" $ do
            uri <- param "uri"
            let parsedUri :: Maybe URI
                parsedUri = parseURI (TL.unpack uri)
            case parsedUri of
                Just _ -> do
                    shawty <- liftIO shortyGen
                    let shorty = BC.pack shawty
                        uri' = encodeUtf8 (TL.toStrict uri)
                    -- first shawty exercise: prevent overwriting of short URIs
                    -- aka collision detection
                    oldShawty <- liftIO (getURI rConn shorty)
                    case oldShawty of
                      Right (Just bs) -> 
                          html (shortyWontOverwrite bsLol)
                            where bsLol :: TL.Text
                                  bsLol = TL.fromStrict (decodeUtf8 bs)
                      otherwise -> do
                        resp <- liftIO (saveURI rConn shorty uri')
                        html (shortyCreated resp shawty)
                Nothing -> text (shortyAintUri uri)
    get "/:short" $ do
        short <- param "short"
        uri <- liftIO (getURI rConn short)
        case uri of
          Left reply -> text (TL.pack (show reply))
          Right mbBS -> case mbBS of
                          Nothing -> text "uri not found"
                          Just bs -> html (shortyFound tbs)
                            where tbs :: TL.Text
                                  tbs = TL.fromStrict (decodeUtf8 bs)

main = do
-- exercise in chapter 22: use ReaderT to make rConn (the database connection) available
-- I don't see how ReaderT would abstract out this part, but we can use it below. hm.
rConn <- R.connect R.defaultConnectInfo
scotty 3000 (runReaderT app rConn)

-- original code:
-- rConn <- R.connect R.defaultConnectInfo
-- scotty 3000 (app rConn)
