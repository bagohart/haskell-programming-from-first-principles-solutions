module Main where

import Lib
import Text.Read
import Control.Monad.Trans.State
import System.Random
import Data.Maybe

-- First attempt: Naive implementation: don't use StateT or even State at all

{-
Exit condition: 21 points for one player
Allowed numbers: provide 1 to 5, guess 2 to 10.

-}

data GameState = GameState {
                            humanScore :: Int 
                           , computerScore :: Int 
                           } deriving (Eq, Show)

data Winner = Human | Computer | Draw deriving (Eq, Show)

gameStartConfig :: GameState
gameStartConfig = GameState 0 0

computerWins :: GameState -> GameState
computerWins (GameState human computer) = GameState human (computer + 1)

humanWins :: GameState -> GameState
humanWins (GameState human computer) = GameState (human + 1) computer

updateScore :: GameState -> Winner -> GameState
updateScore score Draw = score
updateScore score Human = humanWins score
updateScore score Computer = computerWins score

gameOver :: GameState -> Maybe Winner
-- gameOver (GameState human computer) = if human == 16 then Just Human else if computer == 16 then Just Computer else Nothing
gameOver (GameState human computer) = if human == 1 then Just Human else if computer == 1 then Just Computer else Nothing

-- There is state here, but it is trapped in a secret global variable, so I needn't bother with it
getComputerInput :: IO (Int, Int)
getComputerInput = do
    putStrLn "The computer plays, too..."
    finger <- getStdRandom (randomR (1,5))
    -- this implementation is extremely stupid because the guess can be lower than the finger :-)
    guess <- getStdRandom (randomR (2,10))
    putStrLn ("Finger: " ++ show finger ++ ", Guess: " ++ show guess)
    return (finger, guess)

-- applicative notation is sooooo helpful *_*
validFinger :: Int -> Bool
validFinger = (&&) <$> (>=1) <*> (<=5)

-- applicative notation is sooooo helpful *_*
validGuess :: Int -> Bool
validGuess = (&&) <$> (>=2) <*> (<=10)

-- ... this looks like too much repetition o_O I could look them with a condition. hm. That would probably improve this a lot.
-- But not sure about the error message then.
-- Ask the player to show a finger repeatedly until it's correct
getPlayerFinger :: IO Int
getPlayerFinger = do
    putStrLn "Show a finger, please. (Enter a number from 1 to 5)."
    mi <- (readMaybe :: [Char] -> Maybe Int) <$> getLine
    x' <- return $ mi >>= (\i -> if validFinger i then Just i else Nothing)
    case x' of
      Just x'' -> return x''
      Nothing -> do
          putStrLn "No, this was invalid."
          getPlayerFinger

getPlayerGuess :: IO Int
getPlayerGuess = do
    putStrLn "Enter a guess, please. (Enter a number from 2 to 10)."
    mi <- (readMaybe :: [Char] -> Maybe Int) <$> getLine
    x' <- return $ mi >>= (\i -> if validGuess i then Just i else Nothing)
    case x' of
      Just x'' -> return x''
      Nothing -> do
          putStrLn "No, this was invalid."
          getPlayerGuess

getPlayerInput :: IO (Int, Int)
getPlayerInput = do
    f <- getPlayerFinger
    g <- getPlayerGuess
    return (f,g)


playOneRound :: IO Winner
playOneRound = do
    (pf,pg) <- getPlayerInput
    (cf,cg) <- getComputerInput
    let sum = pf + cf 
    putStrLn ("The sum is: " ++ show sum)
    let winner
            | pg == sum && cg == sum = Draw
            | pg == sum && cg /= sum = Human
            | pg /= sum && cg == sum = Computer
            | pg /= sum && cg /= sum = Draw
    return winner


playTheGame :: GameState -> IO GameState
playTheGame score = do
    w <- playOneRound
    putStrLn ("Winner of this round: " ++ show w)
    let newScore = updateScore score w
    putStrLn ("Total Score: Human: " ++ show (humanScore newScore) ++ " Computer: " ++ show (computerScore newScore)) 
    if isJust . gameOver $ newScore then
           return newScore
       else 
            playTheGame newScore

naiveImplementation :: IO ()
naiveImplementation = do
    putStrLn "Let's play Morra."
    let startScores = gameStartConfig
    endScores <- playTheGame startScores
    putStrLn ("And the winner is ..." ++ show (fromJust $ gameOver endScores))
    return ()

-- exercise 1: use StateT to implement the game.

-- The state is the scores of the player. The computed value is... the winner of the current round. or the player with more points? hm.
-- Or Maybe Winner?
-- Or just also the state again?
-- Sketched usage: do a round. check the result, do another round if necessary. the state shouldn't be passed explicitly here,
-- otherwise why would I even use the state thingy?

oneRound :: StateT GameState IO (Maybe Winner)
oneRound = StateT $ \state -> do
    w <- playOneRound
    putStrLn ("Winner of this round: " ++ show w)
    let newScore = updateScore state w
    putStrLn ("Total Score: Human: " ++ show (humanScore newScore) ++ " Computer: " ++ show (computerScore newScore)) 
    return (gameOver newScore, newScore)

allRounds :: StateT GameState IO (Maybe Winner)
allRounds = do
    winnor <- oneRound
    if isJust winnor then return winnor else allRounds

-- there is iterateWhile from some monad loops package on hackage. this probably wraps the above recursion like this:
-- allRounds' :: StateT GameState IO Winner
-- allRounds' = fromJust <$> iterateWhile isJust oneRound

exercise1 :: IO ()
exercise1 = do
    putStrLn "Let's play Morra."
    winner <- fromJust . fst <$> runStateT allRounds gameStartConfig
    putStrLn ("And the winner is... " ++ show winner)
    return ()

-- exercise 3: I could use IORef for this just to see the world burn :)
-- or I just add more stuff to the state. Instead of a score, I get a triple
-- (score, last 2 moves (store as a list, so it can have zero/one moves), 3-gram table)
-- Then, the getComputerInput thingy becomes a bit more complex, but why not.
-----

main :: IO ()
main = naiveImplementation
