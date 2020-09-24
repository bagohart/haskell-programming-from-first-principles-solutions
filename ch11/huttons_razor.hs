data Expr = Lit Integer | Add Expr Expr

foldExpr :: (a -> a -> a) -> (Integer -> a) -> Expr -> a
foldExpr _ g (Lit i) = g i
foldExpr f g (Add e1 e2) = f (foldExpr f g e1) (foldExpr f g e2)

eval :: Expr -> Integer
eval = foldExpr (+) id

printExpr :: Expr -> String
printExpr = foldExpr (\e1 e2 -> "Add (" ++ e1 ++ ") (" ++ e2 ++ ")") (\n -> "Lit " ++ show n)

e1 = Add (Lit 1) (Lit 9001)
