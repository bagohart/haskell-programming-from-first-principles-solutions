module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- Six-sided die
data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do 
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> (state (randomR (1,6)))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where 
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
               in go (sum + die) (count + 1) nextGen

-- 1.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
    where 
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= limit = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
               in go (sum + die) (count + 1) nextGen

-- 2.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 0 [] g
    where 
        go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count sequence gen
          | sum >= limit = (count, sequence)
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
               in go (sum + die) (count + 1) (sequence ++ [intToDie die]) nextGen

main :: IO ()
main = do
  putStrLn "hello world"
  print $ intToDie 5
