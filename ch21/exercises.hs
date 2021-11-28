import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import System.Directory.Internal.Prelude (Applicative)

-- class (Functor t, Foldable t) => Traversable t where
--     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--     traverse f = sequenceA . fmap f
-- So it takes a function that takes x and returns a result inside an applicative
-- it takes a traversable/foldable/functor that contains values of type x
-- then it returns an applicative with the traversable structure containing the result of the first function

-- This is simpler at least, it flips 2 structures/contexts around
-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id

-- fmap :: (a -> b) -> f a -> f b
-- (=<<) :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)

-- Prelude> fmap Just [1, 2, 3]
-- [Just 1,Just 2,Just 3]
-- Prelude> sequenceA $ fmap Just [1, 2, 3]
-- Just [1,2,3]
-- Prelude> sequenceA . fmap Just $ [1, 2, 3]
-- Just [1,2,3]
-- Prelude> traverse Just [1, 2, 3]
-- Just [1,2,3]
-- fmap just over [1, 2, 3], resulting in [Just 1, Just 2, Just 3], then flip the structures around

-- stack ghci --package checkers QuickCheck

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)
