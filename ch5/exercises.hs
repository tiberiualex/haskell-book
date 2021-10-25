{-# LANGUAGE NoMonomorphismRestriction #-}

module CH5 where

-- Type matching
-- a - c, b - d, c - b, d - a, e - e

-- Type arguments
-- 1. a)
-- 2. d)
-- 3. d)
-- 4. c)
-- 5. a)
-- 6. e)
-- 7. f)
-- 8. d)
-- 9. c)

-- Apply yourself
-- 1. myConcat :: [Char] -> [Char]
-- 2. myMult :: Fractional a => a -> a
-- 3. myTake :: Int -> [Char]
-- 4. myCom :: (Ord a, Num a) => a -> Bool -- wrong, Int -> Bool. length produces an Int
-- 5. myAlph :: Char -> Bool

-- Multiple  choices
-- 1. c)
-- 2. a)
-- 3. a) Technically b) is also possible, but there's no details on the implementation
-- 4. c)

-- Determine the type
-- 1. a) Num a => a
--    b) Num a => (a, [Char])
--    c) (Integer, [Char])
--    d) Bool
--    e) Int
--    f) Bool
-- 2. Num a => a
-- 3. z :: Num a => a -> a
-- 4. Fractional a => a
-- 5. f :: [Char]

-- Does it compile
-- 1. No, first expression results in a number, it's fully applied
-- 2. It compiles
-- 3. Third expression won't compile, it tries to apply a number
-- 4. Second expression won't compile, c is not defined
