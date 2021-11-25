-- putStrLn <$> getLine
-- putStrLn :: String -> IO ()
-- getLine :: IO String
-- fmap :: (a      -> b ) -> f  a ->      f      b
-- fmap :: (String -> IO ()) -> IO String -> IO (IO ())
-- The outermost IO structure represents the effects getLine must
-- perform to get you a String that the user typed in
-- The inner IO structure represents the effects that would be
-- performed if putStrLn was evaluated
-- The unit here is the unit that putStrLn returns
-- By using join, we can join the IO layers together
-- join $ putStrLn <$> getLine == getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>=
        \name ->
        putStrLn ("y helo thar: " ++ name)

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
        (\nx ->
            noNegative age' >>=
                (\ax ->
                    noNegative weight' >>=
                        \wx -> weightCheck (Cow nx ax wx)))
