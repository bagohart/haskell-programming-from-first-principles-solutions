module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main = do
    print (roundTrip 4)
    print (id 4)
    print (roundTrip' 44 :: Double)
    print (roundTrip' 44 :: Float)
    print (roundTrip' 44 :: Int)

---
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show
