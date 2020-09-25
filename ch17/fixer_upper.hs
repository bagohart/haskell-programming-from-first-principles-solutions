-- 1.
e1 = const <$> Just "Hello" <*> pure "World"

-- 2.
e2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
