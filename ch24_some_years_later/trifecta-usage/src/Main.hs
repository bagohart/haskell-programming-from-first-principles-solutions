module Main (main) where

import Control.Applicative
import Text.Trifecta as Tri

main :: IO ()
main = do
    putStrLn "hello world"

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char -- I have to add this to turn off some polymorphic wtf error
one = Tri.char '1'

one' = one *> stop

oneTwo :: Parser Char
oneTwo = char '1' *> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo *> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

main1 :: IO ()
main1 = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    pNL "oneTwoEof:"
    testParse oneTwoEof

-- Exercises: Parsing Practice

oneTwoEof :: Parser Char
oneTwoEof = oneTwo <* eof

oneString :: Parser String
oneString = string "1"

oneTwoString :: Parser String
oneTwoString = string "12"

oneTwoThreeString :: Parser String
oneTwoThreeString = string "123"

-- not sure what the book wants. maybe this?
allStrings :: Parser String
allStrings = oneString <|> oneTwoString <|> oneTwoThreeString

-- try this with
-- ghci> parseString (slowString "aha") mempty "ahanope"
slowString :: String -> Parser String
slowString "" = pure ""
slowString (x : xs) = (:) <$> char x <*> slowString xs

-- parsing free jazz
-- kA, nix interessantes

-- parsing fractions
-- es gibt "fail" im parser, das kann man schonmal benutzen.
-- z.b. um fractions "/ 0" abzufangen

unitOfSuccess = parseString (integer <* eof) mempty "123" -- exercise
unitOfSuccess' = parseString (integer <* eof) mempty "123abc" -- exercise

-- Alternative wird hier noch erklärt
-- und quasiquotes. sowas wie raw multiline strings?
-- skipMany, um whitespace zu überspringen
-- "terminal newlines sind irgendwie nervig". uh oh.
-- `try` scheint relevant, was auch immer es ist. ... oh. anscheinend funktioniert <|> und failure gar nicht mit backtracking. sagt chatGPT. lol. sondern wenn irgendwas consumed wurde vom input, dann bleibt es consumed. <|> wird nicht aufgerufen. try muss explizit aufgerufen werden. hm...
-- .ini parser
-- tokenizing: was sind tokens?
-- polymorphic parsers: parser bauen, die auch mit anderen parser librarys benutzt werden können o_O
-- failure and backtracking: try und <?> im detail
-- marshalling: aeson, lazy vs strict bytestrings,
--
-- ... das sieht alles eigentlich noch ziemlich praktisch aus. ist wahrscheinlich ne gute Idee, das mal in Ruhe noch durchzugehen. Die exercises am Ende sind vielleicht nicht so relevant, aber der Rest sieht praktisch und übertragbar aus.
--
-- ... hm. hab es mal in ruhe durchgelesen, aber wirklich hilfreich sind die erklärungen nicht. zu aeson gibt es code-beispiele, aber auch nicht viel an Erklärungen. Die magischen Symbole werden auch unerklärt gelassen.
-- Das hat jetzt eigentlich nicht viel gebracht.
-- Das quasiquoting ist net...
-- => damit hat haskell buch jetzt glaub ich fertig.
