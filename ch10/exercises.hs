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

