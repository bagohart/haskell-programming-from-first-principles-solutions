lefts' :: [Either a b] -> [a]
lefts' = concat . foldr ((:) . filtor) []
    where filtor (Left l) = [l]
          filtor (Right _) = []

rights' :: [Either a b] -> [b]
rights' = concat . foldr ((:) . filtor) []
    where filtor (Right r) = [r]
          filtor (Left _) = []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right r) = Just $ f r

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ g (Right r) = g r

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)
