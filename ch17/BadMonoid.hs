module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- data Bull =
--       Fools
--     | Twoo
--     deriving (Eq, Show)

-- instance Arbitrary Bull where
--     arbitrary =
--         frequency [(1, return Fools)
--                   , (1, return Twoo)]

-- instance Monoid Bull where
--     mempty = Fools
--     mappend _ _ = Fools

-- instance EqProp Bull where (=-=) = eq

-- applicative
--     :: ( Show a, Show (m a), Show (m (a -> b))
--         , Show (m (b -> c)), Applicative m
--         , CoArbitrary a, EqProp (m a)
--         , EqProp (m b), EqProp (m c)
--         , Arbitrary a, Arbitrary b
--         , Arbitrary (m a)
--         , Arbitrary (m (a -> b))
--         , Arbitrary (m (b -> c)))
--         => m (a, b, c) -> TestBatch

xs :: [(String, String, Int)]
xs = [("b", "w", 1)]

-- stack ghci --package checkers QuickCheck

main :: IO ()
main = quickBatch (applicative xs)
