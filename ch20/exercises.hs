import Data.Monoid
import Data.Semigroup

data Optional a = Nada | Yep a
    deriving (Eq, Show)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num c) => p -> t c -> c
product' x = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\e acc -> acc || e == x) False
