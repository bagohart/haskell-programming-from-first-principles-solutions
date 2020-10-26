{-# LANGUAGE BangPatterns #-}

noEval b = 1

manualSeq b = b `seq` 1

bang !b = 1

data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y
