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