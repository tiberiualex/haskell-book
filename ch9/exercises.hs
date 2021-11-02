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
