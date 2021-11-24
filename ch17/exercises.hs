import Control.Applicative

f x =
    lookup x [ (3, "heelo")
             , (4, "hello2")
             , (5, "noooo")]

g y =
    lookup y [ (7, "sup?")
             , (8, "veigar")
             , (9, "malzahar")]
