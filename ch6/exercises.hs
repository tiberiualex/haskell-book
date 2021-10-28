import GHC.Classes (Eq)
import Data.List

-- Eq Instances
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two c d) = a == c && b == d

data StringOrInt =
      TisAnInt   Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a ==b
    (==) _ _ = False

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair c d) = a == c && b == d

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple c d) = a == c && b == d

data Which a =
      ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne b) = a == b
    (==) (ThatOne a) (ThatOne b) = a == b
    (==) _ _ = False

data EitherOr a b =
      Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello b) = a == b
    (==) (Goodbye a) (Goodbye b) = a == b
    (==) _ _ = False

-- Will they work
-- 1. Yes. Length returns an Int, Int implements Ord
-- 2. Yes
-- 3. No. Type mismatch
-- 4. Yes.

-- Multiple choices
-- 1. c)
-- 2. a) b)
-- 3. a)
-- 4. c)
-- 5. a)

-- Does it typecheck?
-- 1. No, type Person doesn't derive or implement Show. It's not a Show instance
-- 2. No, Mood doesn't derive or implement Eq. It's not an Eq instance
-- 3. a) Mood values
--    b) Type error. settleDown is restricted to Mood values
--    c) It wouldn't work, Mood doesn't have an instance of Ord
-- 4. It does typecheck.
type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

-- Given a datatype declaration, what can we do?
-- 2 and 3 typecheck.
-- 1 doesn't because Papu data constructor is called with String and Bool, not Rocks and Yeah
-- 4 doesn't typecheck because Rocks doesn't have an instance of Eq

-- Match the types
-- 2. No
-- 3. Yes
-- 4. Yes
-- 5. Yes
-- 6. Yes
-- 7. No
-- 8. No
-- 9. Yes
-- 10. Yes
