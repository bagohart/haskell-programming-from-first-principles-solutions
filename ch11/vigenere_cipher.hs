import Data.Char
import Data.Bool

shiftUpper :: Int -> Char -> Char
shiftUpper n = 
      chr
    . (\c -> bool c (c - 26) (c > ord 'Z'))
    . (+(n `mod` 26))
    . ord
--this looks weird but I like it (:

shiftLower :: Int -> Char -> Char
shiftLower n =
      chr
    . (\c -> bool c (c - 26) (c > ord 'z'))
    . (+(n `mod` 26))
    . ord

shift :: Int -> Char -> Char
shift n c
  | isLower c = shiftLower n c
  | isUpper c = shiftUpper n c
  | otherwise = c

-- so far taken from the caesar cipher

vigenere :: String -> String -> String
vigenere keyword input = zipWith encrypt (cycle (map toLower keyword)) input

encrypt :: Char -> Char -> Char
encrypt key s = shift (ord key - ord 'a') s

unvigenere :: String -> String -> String
unvigenere keyword input = zipWith decrypt (cycle (map toLower keyword)) input

decrypt :: Char -> Char -> Char
decrypt key s = shift ((26 - (ord key - ord 'a')) `mod` 26) s
