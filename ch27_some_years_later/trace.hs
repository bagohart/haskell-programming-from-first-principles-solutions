import Debug.Trace (trace)

inc = (+ 1)
twice = inc . inc

howManyTimes =
    inc
        (trace "I got eval'd" (1 + 1))
        + twice (trace "I got eval'd" (1 + 1))

howManyTimes' =
    let onePlusOne = trace "I got eval'd" (1 + 1)
     in inc onePlusOne + twice onePlusOne

 -- ... p. 1066 bei `ManualBang`, `DoesntForce` weiter machen. (das hab ich noch nicht gelesen!
