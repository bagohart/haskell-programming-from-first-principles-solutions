module Main where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneEof = one >> eof >> return ' '

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
oneTwoEof = oneTwo >> eof >> return ' '

oneS :: Parser String
oneS = string "1" 

oneTwoS :: Parser String
oneTwoS = string "12" 

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

oneS' :: Parser String
oneS' = char '1' >>= \c -> return [c]

oneTwoS' :: Parser String
oneTwoS' = do
    c1 <- char '1'
    c2 <- char '2'
    return [c1,c2]

oneTwoThreeS' :: Parser String
oneTwoThreeS' = do
    c1 <- char '1'
    c2 <- char '2'
    c3 <- char '3'
    return [c1,c2,c3]

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneEof:"
    testParse oneEof
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    pNL "oneTwoEof:"
    testParse oneTwoEof
    pNL "oneS:"
    testParse' oneS
    pNL "oneTwoS:"
    testParse' oneTwoS
    pNL "oneTwoThreeS:"
    testParse' oneTwoThreeS
    pNL "oneS':"
    testParse' oneS'
    pNL "oneTwoS':"
    testParse' oneTwoS'
    pNL "oneTwoThreeS':"
    testParse' oneTwoThreeS'

-- Importantly, it's not a given... (955)
