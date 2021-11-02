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
