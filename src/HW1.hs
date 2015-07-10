-- https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

module HW1 where

-- Validating Credit Card Numbers

{-  Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is dou-
bled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6]

Add the digits of the doubled values and the undoubled dig-
its from the original number. For example,
[2,3,16,6] becomes 2+3+1+6+6 = 18

Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.
-}

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
  where toDigitsRev n | n > 0 = let (rest, fstNum) = n `divMod` 10 in
                        fstNum : toDigitsRev rest
                      | otherwise = []

{-
-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev d
  | d <= 0    = []
  | otherwise = rem d 10 : toDigitsRev (quot d 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther xs'@(x:xs) = if even len
                              then merge (map (*2) even') odd'
                              else x : doubleEveryOther xs
  where even' = [xs'!!n | n<-idxs, even n]
        odd'  = [xs'!!n | n<-idxs, odd  n]
        idxs  = [0..len-1]
        len   = length xs'

        merge [] []         = []
        merge (a:as) []     = a:merge as []
        merge [] (b:bs)     = b:merge [] bs
        merge (a:as) (b:bs) = a:b:merge as bs

{-
-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:ys) = x : 2*y : doubleEveryOther ys
doubleEveryOther xs       = xs
-}

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

{-
-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev
--}

validate :: Integer -> Bool
validate n = x `mod` 10 == 0
  where x = sumDigits . doubleEveryOther . toDigits $ n

{-
-- Exercise 4
validate :: Integer -> Bool
validate = (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigitsRev
-}
