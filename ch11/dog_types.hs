data PugType = PugData
data DogueDeBordeaux doge = DogueDeBordeaux doge
data HuskyType a = Huskydata
data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- 1. Doggies is a type constructor
-- 2. Doggies :*: * -> *
-- 3. Doggies String :*: *
-- 4. Husky 10 :: (Num a) => Doggies a
-- 5. Husky (10 : Integer) :: Doggies Integer
-- 6. Mastiff "Scooby Doo" :: Doggies String
-- 7. Both
-- 8. DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux "doggie!" :: DogueDeBordeaux String
