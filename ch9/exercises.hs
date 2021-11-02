-- EnumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []

eftGeneric :: (Ord a, Enum a) => a-> a -> [a]
eftGeneric a b =
    case compare a b of
        GT -> []
        LT -> a : eftGeneric (succ a) b
        EQ -> [a]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftGeneric

eftInt :: Int -> Int -> [Int]
eftInt = eftGeneric

eftChar :: Char -> Char -> [Char]
eftChar = eftGeneric

-- Thy Fearful Symmetry
--1.

myWords :: [Char] -> [[Char]]
myWords "" = []
myWords (' ':xs) = myWords xs
myWords str =
    takeWhile noSpace str : myWords (dropWhile noSpace str)
        where noSpace = (' ' /=)

-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines ('\n':xs) = myLines xs
myLines str =
    takeWhile noNewLine str : myLines (dropWhile noNewLine str)
        where noNewLine = ('\n' /=)

-- 3.
separateBy :: Char -> [Char] -> [[Char]]
separateBy _ "" = []
separateBy c ax@(x:xs)
    | c == x = separateBy c xs
    | otherwise =
            takeWhile noChar ax : separateBy c (dropWhile noChar ax)
                where noChar = (c /=)

-- Square cubes
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
tuples = [(x, y) | x <- mySqr, y <- myCube]
tuples2 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

-- Will it blow up?
-- 1. Yes
-- 2. No
-- 3. Yes
-- 4. No
-- 5. Yes
-- 6. No
-- 7. Yes
-- 8 . No
-- 9. No
-- 10. Yes

-- Is it normal form?
-- 1. NF
-- 2. WHNF
-- 3. Neither
-- 4. Neither
-- 5. Neither?
-- 6. WHNF
-- 7. WHNF

-- More bottom exercises
-- 1. Error
-- 2. Will return value
-- 3. Error
-- 4. It takes a string and returns a list showing which letters are vowels and which aren't
-- 5. a) [1, 4, 9 ..] squares of the numbers 1 to 10
--    b) [1, 10, 20]
--    c) [15, 15, 15]
