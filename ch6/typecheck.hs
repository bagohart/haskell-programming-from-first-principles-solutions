-- 1.
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- needs this:
instance Show Person where
    show (Person b) = "Person " ++ show b

-- 2.
data Mood = Blah | Woot deriving (Eq, Show) -- needs Eq, not Show

settleDown x = if x == Woot then Blah else x

-- 3.
-- a) Woot and Blah...
-- b) doesn't work, 9 is not instance of Mood
-- c) does not work, is not instance of Ord, although it is instance of Eq

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" -- this should work - it is a function :: String -> Sentence
s2 = Sentence "Julie" "loves" "dogs"


