-- Mood swing
data Mood = Blah | Woot deriving Show
-- 1. Mood
-- 2. Blah and Woot

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

