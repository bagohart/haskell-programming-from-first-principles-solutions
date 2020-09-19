a = flip (++) "!"
b = flip (!!) 4
c = drop 9

thirdLetter :: String -> Char
thirdLetter = flip (!!) 2

letterIndex :: Int -> Char
letterIndex = (!!) input
    where input = "Curry is awesome!"

-- wtf
rvrs :: String -> String
rvrs input = drop 9 input ++ " " ++ (take 2 . drop 6) input ++ " " ++ take 5 input
    -- where input = "Curry is awesome!"
