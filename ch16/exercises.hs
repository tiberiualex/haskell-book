-- Kinds
-- 1. kind *
-- 2. They're both kind * -> *
-- 3. kind * -> * -> *

-- Heavy lifting
-- 1.
a :: [Int]
a = fmap (+1) (read "[1]" :: [Int])

-- 2.
b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c :: Integer -> Integer
c = fmap (*2) (\x -> x - 2)
c1 :: Integer
c1 = c 1

-- 4.
d :: Integer -> [Char]
d =
    fmap ((return '1' ++) . show)
    (\x -> [x, 1..3])

d0 :: [Char]
d0 = d 0

-- 5.

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        x = fmap (("123"++) . show) ioi
        changed = fmap read x
        in fmap (*3) changed


f :: IO Integer
f = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . (("123"++) . show)) ioi
        in fmap (*3) changed
