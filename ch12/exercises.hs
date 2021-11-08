-- Chapter exercises
-- Determine the kinds
-- 1. a :: *
-- 2. a :: *, f :: * -> *

-- String processing
-- 1.

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

mapperF x = case notThe x of
                Nothing -> " a"
                Just a -> ' ' : a

replaceThe :: String -> String
replaceThe "" = ""
replaceThe s = tail $ concatMap mapperF $ words s

-- 2.

isVowel :: Char -> Bool
isVowel l = l `elem` "aeiou"

vowelAfterThe :: [Char] -> [Char] -> Bool
vowelAfterThe x1 x2 =
    (x1 == "the") && isVowel (head x2)

countTheBeforeVowel :: [String] -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel [x] = 0
countTheBeforeVowel (x:xs) =
    n + countTheBeforeVowel xs
    where n = if vowelAfterThe x (head xs) then 1 else 0

countTheBeforeVowel' :: String -> Integer
countTheBeforeVowel' = countTheBeforeVowel . words

-- Validate the word
newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels :: String
vowels = "aeiou"
vw :: String -> Int
vw = foldr (\x acc -> if x `elem` "aeiou" then acc + 1 else acc) 0

mkWord :: String -> Maybe Word'
mkWord s = if moreVowels
           then Nothing
           else Just (Word' s)
           where moreVowels = vw s > length s - vw s + 1

-- Natural numbers
data Nat =
      Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

convertToNat :: Integer -> Nat
convertToNat 0 = Zero
convertToNat n = Succ (convertToNat (n - 1))

integerToNat :: Integer -> Maybe Nat
integerToNat n = if n < 0 then Nothing else Just (convertToNat n)

-- Library for Maybe

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee x f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x = mayybee x id

-- todo: Either exercises
