hypo :: IO ()
hypo = do
    let x :: Integer
        x = undefined
    s <- getLine
    case s of
      "hi" -> print x
      _ -> putStrLn "hello"

hypo' :: IO ()
hypo' = do
    let x :: Integer
        x = undefined
    s <- getLine
    case x `seq` s of
      "hi" -> print x
      _ -> putStrLn "hello"

hypo'' :: IO ()
hypo'' = do
    let x :: Integer
        x = undefined
    s <- x `seq` getLine
    case s of
      "hi" -> print x
      _ -> putStrLn "hello"

xD :: Int
xD = let z = 5:undefined; b = True in case z `seq` b of
                                        True -> 7
                                        False -> 10

xD' :: Int
xD' = let z = 2+undefined; b = True in case z `seq` b of
                                        True -> 7
                                        False -> 10

