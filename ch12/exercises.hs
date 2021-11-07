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
