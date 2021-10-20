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
