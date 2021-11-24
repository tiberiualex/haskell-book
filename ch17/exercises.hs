import Control.Applicative
import Data.List (elemIndex)

f x =
    lookup x [ (3, "heelo")
             , (4, "hello2")
             , (5, "noooo")]

g y =
    lookup y [ (7, "sup?")
             , (8, "veigar")
             , (9, "malzahar")]

h z =
    lookup z [(2, 3), (5, 6), (7, 8)]

m x =
    lookup x [(4, 10), (8, 13), (1, 9001)]

-- Lookups

-- 1.
added :: Maybe Integer
added =
    (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

-- 3.
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x3 y3

maxed' :: Maybe Int
maxed' = max' <$> x3 <*> y3

-- 4.
xs4 = [1, 2, 3]
ys4 = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs4 ys4

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs4 ys4


summed :: Maybe Integer
summed = fmap sum $ (,) <$> x4 <*> y4

-- (,) <$> x4 <*> y4 results in Just (6, 5)
-- then just fmap sum over that

-- Identity instance
newtype Identity' a = Identity' a
    deriving (Eq, Ord, Show)

instance Functor Identity' where
    fmap f (Identity' a) = Identity' (f a)

instance Applicative Identity' where
    pure = Identity'
    (<*>) (Identity' f) (Identity' a) = Identity' (f a)

newtype Constant' a b =
    Constant' { getConstant' :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
    fmap _ (Constant' e) = Constant' e

instance Monoid a => Applicative (Constant' a) where
    pure _ = Constant' mempty
    Constant' x <*> Constant' y = Constant' (x `mappend` y)

-- Fixed upper
something :: Maybe [Char]
something = const <$> Just "Hello" <*> pure "World"

zupple = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]