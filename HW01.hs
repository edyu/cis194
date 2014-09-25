{-
 Name: Ed Yu
 Collaborators: none
 Notes:
 -}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

-- Exercise 1
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2
toDigitsReversed x | x <= 0    = []
                   | otherwise = lastDigit x : toDigitsReversed (dropLastDigit x)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsReversed x)

-- Exercise 3
apply f xs | null xs   = []
           | otherwise = (f (head xs)) : dontApply f (tail xs)

dontApply f xs | null xs   = []
               | otherwise = (head xs) : apply f (tail xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (dontApply (* 2) (reverse xs))
