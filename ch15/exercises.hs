import Data.Monoid
import Test.QuickCheck

-- class Monoid m where
-- mempty :: m
-- mappend :: m -> m -> m
-- mconcat :: [m] -> m
-- mconcat = foldr mappend mempty

-- mappend mempty x = x
-- -- right identity
-- mappend x mempty = x
-- -- associativity
-- mappend x (mappend y z) =
-- mappend (mappend x y) z
-- mconcat = foldr mappend mempty

-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b) => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c) => (a, b, c)

-- sigh: https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in
-- tl;dr Semigroup is now a superclass of Monoid and mappend is only for legacy reeasons now, so use <>

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)
instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    Nada <> o = o
    o <> Nada = o
    (Only x) <> (Only y) = Only (x <> y)
-- stack ghci --package QuickCheck
-- type S = String
-- type B = Bool
-- quickCheck (monoidAssoc :: S -> S -> S -> B)
-- verboseCheck (monoidAssoc :: S -> S -> S -> B) to show the types/values being tested
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> ( b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- main :: IO ()
-- main = do
--     let ma = monoidAssoc
--         mli = monoidLeftIdentity
--         mlr = monoidRightIdentity
--         quickCheck (ma :: Bull -> Mappend)
--         quickCheck (mli :: Bull -> Bool)
--         quickCheck (mlr :: Bull -> Bool)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivialAssoc :: IO ()
testTrivialAssoc = verboseCheck (semigroupAssoc :: TrivAssoc)

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdenAssoc = Identity All -> Identity All -> Identity All -> Bool

testIdenAssoc :: IO ()
testIdenAssoc = verboseCheck (semigroupAssoc :: IdenAssoc)

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc = Two All Trivial -> Two All Trivial -> Two All Trivial -> Bool

testTwoAssoc :: IO ()
testTwoAssoc = quickCheck (semigroupAssoc :: TwoAssoc)

newtype BoolConj =
    BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

type BoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
    arbitrary = do
        x <- elements [True, False]
        return (BoolConj x)

testBoolConjAssoc :: IO ()
testBoolConjAssoc = quickCheck (semigroupAssoc :: BoolAssoc)

newtype BoolDisj =
    BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- elements [True, False]
        return (BoolDisj x)

testBoolDisjAssoc :: IO ()
testBoolDisjAssoc = quickCheck (semigroupAssoc :: BoolDisjAssoc)
