-- 1.
-- this implementation looks a bit stupid, but this is what the book suggests. hm.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . map (replace . notThe) . words
    where replace Nothing = "a"
          replace (Just w) = w

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = count $ zip theWords (tail theWords)
    where theWords = words text
          count = fromIntegral . length . filter (\(first, second) -> first == "the" && head second `elem` "aeiou")

-- 3.
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` "aeiou")
