-- Scope
-- 1. yes
-- 2. no
-- 3. no
-- 4. yes

-- Syntax error
x = (++) [1, 2, 3] [4, 5, 6]
y = "<3" ++ " Haskell"
-- This example is already correct
z = concat ["<3", " Haskell"]

-- Chapter exercices
-- 1.
a1 = concat [[1, 2, 3], [4, 5, 6]] -- correct
b1 = (++) [1, 2, 3] [4, 5, 6]
c1 = (++) "hello" " world" -- correct
d1 = ["hello" ++ " world"]
e1 = "hello" !! 4
f1 = (!!) "hello" 4 --correct
g1 = take 4 "lovely"
h1 = take 3 "awesome" --correct

--2. a-d, b-c, c-e, d-a, e-b

-- Building functions
a2 = "Curry is awesome" ++ "!"
b2 = "Curry is awesome!" !! 4
c2 = drop 9 "Curry is awesome!"

concatenateEx :: String -> String
concatenateEx str = str ++ "!"

takeFifth :: String -> Char
takeFifth str = str !! 4

dropNine :: String -> String
dropNine = drop 9

thirdLetter :: String -> Char
thirdLetter str = str !! 2

letterIndex :: Int -> Char
letterIndex index = "Curry is awesome!" !! index

rvsr :: String -> String
rvsr x = ((take 7) . (drop 9) $ x) ++ ((drop 5) . (take 9) $ x) ++ take 5 x

