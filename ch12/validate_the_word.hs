newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
consonants = "bcdfghjklmnpqrstvwxyz"

count :: String -> String -> Int
count things = length . filter (`elem` things)

mkWord :: String -> Maybe Word'
mkWord word
  | numVowels > numConsonants = Nothing
  | numConsonants <= numVowels = Just $ Word' word
  | otherwise = Just $ Word' word
    where numVowels = count vowels word
          numConsonants = count consonants word
