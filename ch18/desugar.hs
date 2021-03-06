bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = 
    putStrLn "name pls:" >>
    getLine >>= putStrLn . ("y helo thar"++)

twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
    (putStrLn "name pls:" >>
    getLine) >>=
    (
        \name ->
        putStrLn "age pls:" >>
        getLine >>=
        (
            \age ->
            putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")
        )
    )
