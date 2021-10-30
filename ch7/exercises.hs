-- Grab bag
-- 1. They're all equivalent
-- 2. d)
-- 3.
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = (\n -> n + 1)

addFive x y = (if x > y then y else x) + 5
addFive' x = \y -> (if x > 5 then y else x) + 5

mflip f = \x -> \y -> f y x
mflip' f x y = f y x

-- Variety pack
-- 1. a) k :: (a, b) -> a
--    b) Num a => a
--    c) [Char]. No, it's not the same time as k1 and k3
--    d) k2 and k3
-- 2.

f2 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f2 (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case practice
functionC :: Ord p => p -> p -> p
functionC x y = if (x > y) then x else y

functionC' :: Ord p => p -> p -> p
functionC' x y = case x > y of
                    True -> x
                    False -> y

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' :: Integral p => p -> p
ifEvenAdd2' n = case even n of
                    True -> n + 2
                    False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

-- Arftul dodgy

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- 1. 1.
-- 2. 11
-- 3. 22
-- 4. 21
-- 5. 12
-- 6. 11
-- 7. 21
-- 8. 21
-- 9. 22
-- 10. 31
-- 11. 23

