module Cipher where

import Data.Char

cipherChar :: Int -> Char -> Char
cipherChar i c
    | c < 'a' || c > 'z' = c
    | otherwise =
        let ai = ord 'a'
            zi = ord 'z'
            ci = ord c + i
        in
            if ci <= zi
                then chr ci
                else chr $ ci - zi + ai - 1

cipherChar' :: Int -> Char -> Char
cipherChar' i c
    | c < 'a' || c > 'z' = c
    | otherwise =
        let ai = ord 'a'
            zi = ord 'z'
            ci = ord c + i
        in
            if ci >= ai
                then chr ci
                else chr $ zi + ci - ai + 1


cipherString :: Int -> [Char] -> [Char]
cipherString i = map (cipherChar i)

decipherString :: Int -> [Char] -> [Char]
decipherString i = map (cipherChar' (-i))

main :: IO ()
main = do
    putStrLn "Enter the word to encode: "
    word <- getLine
    putStrLn $ "The encoded word is: " ++ cipherString 4 word
    return ()