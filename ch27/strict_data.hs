{-# LANGUAGE Strict #-}

blah x = 1
main = print (blah undefined)

force  x = 1
noForce ~x = 1
