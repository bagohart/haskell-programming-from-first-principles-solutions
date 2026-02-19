import Debug.Trace

f :: a -> Int
f = trace "f" const 1

g :: a -> Int
g _ = trace "g" 1
