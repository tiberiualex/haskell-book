{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

import Test.QuickCheck
import Test.QuickCheck.Function

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

-- QuickCheck functor exercises

-- stack ghci --package QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g . fmap f) x == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a =
    Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

type IntToInt = Fun Int Int

type IdInt = Identity Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

testIdentityFunctor :: IO ()
testIdentityFunctor = quickCheck (functorCompose' :: IdInt)

data Pair a b =
    Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

type TupleInt = Pair Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

testTupleFunctor :: IO ()
testTupleFunctor = quickCheck (functorCompose' :: TupleInt)

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeInt = Three Int Int Int -> IntToInt -> IntToInt -> Bool

testThreeFunctor :: IO ()
testThreeFunctor = quickCheck (functorCompose' :: ThreeInt)

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three' a b c)

type ThreeInt' = Three' Int Int -> IntToInt -> IntToInt -> Bool

testThreeFunctor' :: IO ()
testThreeFunctor' = quickCheck (functorCompose' :: ThreeInt')
