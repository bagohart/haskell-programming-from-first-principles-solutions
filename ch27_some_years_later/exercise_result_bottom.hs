ghci> snd (undefined,1)
> 1
ghci> let x = undefined
ghci> let y = x `seq` 1 in snd (x,y)
> *** Exception: Prelude.undefined
ghci> length $ [1..5] ++ undefined
> *** Exception: Prelude.undefined
ghci> length $ [1..5] ++ [undefined]
> 6
ghci> const 1 undefined
> 1
ghci> const 1 (undefined `seq` 1)
> 1
ghci> const undefined 1
> *** Exception: Prelude.undefined
