import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
    print (typeOf e)
    putStrLn ("We errored! It was: " ++ show e)

handler2 :: SomeException -> IO ()
handler2 (SomeException e) = do
    putStrLn ("Running main caused an error! It was: " ++ show e)
    writeFile "bbb" "hi?"

main = do
    writeFile "aaa" "hi"
    putStrLn "wrote to file"
    -- writeFile "zzz" "hi?" `catch` handler
    writeFile "zzz" "hi?" `catch` handler2
    putStrLn "wrote to file"
