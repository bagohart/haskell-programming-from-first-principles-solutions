isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee e _ Nothing = e
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe e Nothing = e

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if (length . (filter isNothing) $ xs) > 0 then Nothing else Just $ map unwrap xs
    where unwrap (Just x) = x
