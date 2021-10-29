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
