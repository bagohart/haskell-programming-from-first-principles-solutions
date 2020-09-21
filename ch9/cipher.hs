module Cipher where

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

caesar :: Int -> [Char] -> [Char]
caesar n = map (shift n)

uncaesar :: Int -> [Char] -> [Char]
uncaesar n = caesar (26 - n)
