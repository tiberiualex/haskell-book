module DatabaseProcessing where

import Data.Time
data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
        (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
        , DbNumber 9001
        , DbString "Hello, world!"
        , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
    ]

timeFold :: DatabaseItem -> [UTCTime] -> [UTCTime]
timeFold (DbDate t) acc = t : acc
timeFold _ acc = acc

filterDbDate :: [DatabaseItem]-> [UTCTime]
filterDbDate = foldr timeFold []

numberFold :: DatabaseItem -> [Integer] -> [Integer]
numberFold (DbNumber n) acc = n : acc
numberFold _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr numberFold []

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber