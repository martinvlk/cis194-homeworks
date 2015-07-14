{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW07.Scrabble where

import Data.Monoid
import Data.Char (toUpper)

-- ex3
newtype Score = Score Int
              deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c | cup `elem` "AEIOULNRST" = 1
        | cup `elem` "DG"    = 2
        | cup `elem` "BCMP"  = 3
        | cup `elem` "FHWYV" = 4
        | cup == 'K'         = 5
        | cup `elem` "JX"    = 8
        | cup `elem` "QZ"    = 10
        | otherwise          = 0
  where cup = toUpper c

scoreString :: String -> Score
scoreString = sum . map score

getScore :: Score -> Int
getScore (Score i) = i
