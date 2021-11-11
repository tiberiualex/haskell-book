module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = do
  words <- allWords
  word <- randomWord words
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

type WordList = [String]
allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in      l >= minWordLength
                 && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

data Puzzle =
    Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed _) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (replicate (length s) Nothing ) "" 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s _) c = c `elem` s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c)  = c

zipper :: Char -> Char -> Maybe Char -> Maybe Char
zipper guessed wordChar guessChar =
    if wordChar == guessed
    then Just wordChar
    else guessChar

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSofar s i) c =
    Puzzle word newFilledInSoFar (c : s) (i + x)
    where
        newFilledInSoFar = zipWith (zipper c) word filledInSofar
        x = if newFilledInSoFar == filledInSofar then 1 else 0

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that"
            return puzzle
        (True, _) -> do
            putStrLn "Guessed correct, filling in"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "Guessed wrong"
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed incorrectGuesses) =
    if (incorrectGuesses) >= 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then
        do putStrLn "You win!"
           exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be single character"
