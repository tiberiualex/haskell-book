module ListApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
      Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap = map'

instance Applicative List where
    pure f = Cons f Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    a <*> b = flatMap (\y -> fmap y b) a

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        return (x `Cons` Nil)

instance Eq a => EqProp (List a) where (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

map' :: (a -> b) -> List a -> List b
map' f = fold (\x y -> Cons (f x) y) Nil

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

x = 3 `Cons` (5 `Cons` Nil)
y = 4 `Cons` (11 `Cons` Nil)
x' = 1 `Cons` Nil

plusOne = (+1)

plusTwo = (+2)

makeDouble x = x `Cons` (x `Cons` Nil)

plusNumbers = plusOne `Cons` (plusTwo `Cons` Nil)

testApply = plusNumbers <*> x

z = flatMap makeDouble x

-- [(+1), (+2)] <*> [1, 2]
-- fmap (+1) [1, 2] = [2, 3]
-- fmap (+2) [1, 2] = [3, 4]
-- [2, 3] <> [3, 4]

listApp :: [(List Int, List Int, List Int)]
listApp = [(x, y, x')]

list2 :: List (Int, Int, Int)
list2 = (1, 2, 3) `Cons` Nil

-- stack ghci --package checkers QuickCheck
main :: IO ()
main = quickBatch (applicative list2)
