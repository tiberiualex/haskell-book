{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercises: Dog types
-- 1. Type constructor
-- 2. The kind is * -> *
-- 3. *
-- 4. Num a => Doggies a
-- 5. Doggies Integer
-- 6. Doggies String
-- 7. Both
-- 8. DogueDeBordeaux a
-- 9. DogueDeBordeaux String

data Price = Price Integer
    deriving (Eq, Show)

data Manufacturer =
      Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
      PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. Vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

--4. Exception

-- Cardinality
-- 1. Cardinality is 1
-- 2. Cardinality is 3. It's a sum type with 3 nullary data constructors
-- 3. Int16 cardinality is 65536. We can use minBound and maxBound to find the cardinality
-- 4. Integer is not bounded, has no finite cardinality
-- 5. Int8 cardinality = 2 ^ 8.

-- Example datatype
data Example = MakeExample deriving Show

-- 1. MakeExample :: Example. Can't request the type of Example, it's not a data constructor
-- 2. You get the type information and the typeclasses it implements/derives
-- 3. MakeExample2 :: Int -> Example2
data Example2 = MakeExample2 Int deriving Show


-- Exercises: Logic goats
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany String where
    tooMany s = length s > 42

instance TooMany (Int, String) where
    tooMany (i, s) = length s > 42 && i > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

newtype TooManyIdk = TooManyIdk (Int, String) deriving (Eq, Show, TooMany)

newtype GoatFields =
    GoatFields (Int, Int) deriving (Eq, Show)

instance TooMany GoatFields where
    tooMany (GoatFields (x, y)) = x + y > 42

-- Exercises: Bool
-- 1. The cardinality is 4
-- 2. The cardinality is 256 (the cardinality of Int8) + 2 (the cardinality of Bool) = 258

-- Constructing and deconstructing values

data GuessWhat =
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
                  deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

