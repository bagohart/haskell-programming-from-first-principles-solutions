import Data.Char

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (w:ws) = toUpper w : ws
