module Main where

import Control.Monad (forever) -- [1]
import Data.Char (toLower) -- [2]
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [6]

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
        let l = length (w :: String)
        in l > minWordLength && l < maxWordLength

-- ... eigentlich ein nettes rätsel um mit IO zu spielen, aber ohne HLS macht das keinen Spaß :)
