-- let
half :: Fractional a => a -> a
half x = x / 2

-- let
square :: Num a => a -> a
square x = x * x

piMultiple x = pi * x

-- Parentheisazation
-- 2 + (2 * 3) - 1
-- (^) 10 (1 + 1)
-- (2 ^ 2) * (4 ^ 5) + 1

-- Equivalent expressions
-- 1. yes
-- 2. yes
-- 3. no
-- 4. no
-- 5. no

z = 7
y = z + 8
x = y ^ 2
waxOn = x * value
        where value = 5

triple x = x * 3

waxOff x = triple x
