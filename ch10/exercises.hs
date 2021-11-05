-- Understanding folds
-- 1. b) c)
-- foldr f acc (x:xs) = f x (foldr f acc xs)
-- 2.
-- 3. c)
-- foldl f acc (x:xs) = foldl f (f acc x) xs
-- foldl (flip (*)) 1 [1..3]
-- ((1 * 1) * 2) * 3)
-- (2 * 1) * 3
-- 6

-- 4. a)
-- 5. a) foldr (++) "" ["woot", "WOOT", "woot"]
--    b) foldr max 'a' "fear is the little death"
--    c) foldr (&&) True [False, True]
--    d) foldr (||) False [False, True]
--    e) foldl (flip ((++) . show)) "" [1..5]
--    f) foldr (flip const) 'a' [1..5]

-- Rewriting functions using folds
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny:: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x acc -> (x == a) || acc) False

-- 4.
myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

-- 7.

squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


