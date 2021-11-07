{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Char
import Data.List
import Data.Maybe

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

-- Programmers (constructing values)
data OperatingSystem =
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
                , lang :: ProgLang }
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly =
    Programmer { lang = Agda
               , os = GnuPlusLinux }

allLanguages :: [ProgLang]
allLanguages =
    [Haskell, Agda, Idris, PureScript]

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { lang = a, os = b} | a <- allLanguages, b <- allOperatingSystems ]

-- The Quad (function type is exponential)
data Quad =
      One
    | Two
    | Three
    | Four

-- 1. eQuad :: Either Quad Quad -> It's a sum type, Quad has the cardinality 4, so 4 + 4 = 8
-- 2. prodQuad :: (Quad, Quad) -> Product type, Quad has the cardinality 4, so 4 * 4 = 16
-- 3. funcQuad :: Quad -> Quad -> Function type, Quad has the cardinality 4, so 4 ^ 4 = 256 forms
-- 4. prodTBool (Bool, Bool, Bool) -> Product type, Bool has the cardinality 2, so 2 * 2 * 2 = 8
-- 5. gTwo :: Bool -> Bool -> Bool -> Function type, Bool has the cardinality 2, so (2 ^ 2) ^ 2 = 4 ^ 2 = 16
-- 6. fTwo :: Bool -> Quad -> Quad -> Function type, Bool has  the cardinality 2, Quad has 4, so (2 ^ 4) ^ 4 = 65536

-- Binary trees
data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)


insert' :: Ord a
    => a
    -> BinaryTree a
    -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)


-- Chapter exercises
-- 1. a)
-- 2. c)
-- 3. b)
-- 4. c)

-- As patterns
-- 1.
isSubseqOf :: (Eq a)
    => [a]
    -> [a]
    -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf allx@(x:xs) ally@(y:ys)
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf allx ys

-- f all@(w:ws) = (((toUpper w) : ws), all)
capitalTuple :: String -> (String, String)
capitalTuple [] = ("", "")
capitalTuple all@(w:ws) = (all, toUpper w : ws)

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map capitalTuple (words s)

-- Language exercises
-- 1.
capitalizeWord :: String -> String
capitalizeWord w =
    case w of
        "" -> ""
        (x:xs) -> toUpper x : xs



-- Phone exercise

type Digit = Char
type Presses = Int
type Characters = String
type Key = (Digit, Characters)
type Tap = (Digit, Presses)
type DaPhone = [Key]

phone :: DaPhone
phone =
    [
      ('1', "1")
    , ('2', "ABC2")
    , ('3', "DEF3")
    , ('4', "GHI4")
    , ('5', "JKL5")
    , ('6', "MNO6")
    , ('7', "PQRS7")
    , ('8', "TUV8")
    , ('9', "WXYZ9")
    , ('*', "^")
    , ('0', "+ 0")
    , ('#', ".,")
    ]

convo :: [String]
convo =
    [ "Random question 1",
      "Hello",
      "Random question 2",
      "Qfwg fwgw 323. jHfqw",
      "String 4" ]

createTaps :: DaPhone -> String -> [Tap]
createTaps daPhone = map (fromJust . charToTap daPhone)

-- To fix: some faulty logic in changing the casing
createTapsWithCaseChange :: DaPhone -> String -> [Tap]
createTapsWithCaseChange daPhone = fst . foldr (tapsFolder phone) ([], True)

tapsFolder :: DaPhone -> Char -> ([Tap], Bool) -> ([Tap], Bool)
tapsFolder phone x acc =
    let y = charToTapWithCasingInfo phone (x, snd acc)
    in
        (map fromJust (fst y) ++ fst acc, snd y && snd acc)

charToTap :: DaPhone -> Char -> Maybe Tap
charToTap p c =
    case find (\key -> toUpper c `elem` snd key) p of
        Nothing -> Nothing
        Just (digit, chars) -> Just (digit, presses)
            where presses = (fromJust $ toUpper c `elemIndex` chars) + 1 -- This is safe, as we wouldn't match this case if the character wasn't among the key's characters

charToTapWithCasingInfo :: DaPhone -> (Char, Bool) -> ([Maybe Tap], Bool)
charToTapWithCasingInfo phone (c, lowerCasing) =
    if isAlphaNum c
    then
        case isUpper c /= lowerCasing of
            True -> ([charToTap phone '^', charToTap phone c], not lowerCasing)
            False -> ([charToTap phone c], lowerCasing)
    else ([charToTap phone c], lowerCasing)

createTapsForConversation :: DaPhone -> [String] -> [Tap]
createTapsForConversation phone = concatMap (createTapsWithCaseChange phone)
