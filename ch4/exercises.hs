-- Mood swing
data Mood = Blah | Woot deriving Show
-- 1. Mood
-- 2. Blah and Woot

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

-- Find the mistakes
a1 = not True && True
b1 x = not (x == 6)
c1 = (1 * 2) > 5 -- already correct
d1 = "Merry" > "Happy"
e1 = ["1", "2", "3"] ++ ["look at me!"]

-- Chapter exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1. length :: [a] -> Int. More accurate answer length :: Foldable t => t a -> Int
-- 2. a) 5
--    b) 3
--    c) 2
--    d) 5
-- 3. length returns an Int, / is division for Fractional numbers
-- 4. 6 `div` (length [1, 2, 3])
-- 5. Boolean. True
-- 6. Boolean. False
-- 7. a) works, produces True
--    b) won't work, different types in the list
--    c) works, produces 5
--    d) works, produces False. 8 == 8 produces True, 'a' < 'b' produces False. True && False == False
--    e) Doesn't work, && only works on Boolean values, 9 is a Num

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

myAbs ::  (Num a, Ord a) => a -> a
myAbs x = if x < 0
         then -x
         else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

x = (+)

addOne :: String -> Int
addOne xs = w `x` 1
            where w = length xs


idf :: a -> a
idf x = x

fnd :: (a, b) -> a
fnd (a, b) = a

-- Match types
-- 1. c)
-- 2. b)
-- 3. a)
-- 4. d)