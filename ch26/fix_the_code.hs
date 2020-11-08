import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Control.Applicative

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine -- or just use lift
    guard $ isValid v
    -- ^ this means:
    -- if isValid v then pure () else empty
    -- In this case, the empty is an IO action with return value Nothing, and
    -- pure () is an IO action which does nothing and delivers (),
    -- which means according to the implementation of MaybeT's >>= that the next line will be 'skipped' or not.
    return v

    -- Could be written withoud the guard function as:
    -- if isValid v then MaybeT $ return (Just v) else MaybeT $ return Nothing
    -- this Alternative thing still seems a bit magical to me

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
      Nothing -> putStrLn "MOAR EXCITE"
      Just e -> putStrLn ("Good, was very excite: " ++ e)
    return ()


-- some tests for sanity-checking empty and MaybeT
x :: IO (Maybe String)
x = do
    putStrLn "lol"
    empty
    putStrLn "lol2"
    return Nothing

y :: MaybeT IO String
y = do
    empty
    lift $ putStrLn "lol"
    return "lol"

z :: MaybeT IO String
z = do
    lift $ putStrLn "lol"
    empty
    return "lol"

