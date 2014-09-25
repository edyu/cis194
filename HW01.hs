{-# OPTIONS_GHC -Wall #-}
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
-- Break up a number into its last digit
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Break up a number except its last digit
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2
-- Break apart a number into its digits in reverse order
toDigitsReversed :: Integer -> [Integer]
toDigitsReversed x | x <= 0    = []
                   | otherwise = lastDigit x : toDigitsReversed (dropLastDigit x)

-- Break apart a number into its digits
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsReversed x)

-- Exercise 3
-- Apply a function to every other digit
apply :: (a -> a) -> [a] -> [a]
apply f xs | null xs   = []
           | otherwise = (f (head xs)) : dontApply f (tail xs)

-- Skip the function on every other digit
dontApply :: (a -> a) -> [a] -> [a]
dontApply f xs | null xs   = []
               | otherwise = (head xs) : apply f (tail xs)

-- Double every other digit
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (dontApply (* 2) (reverse xs))

-- Exercise 4
-- Calculate the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (\acc x -> if x >= 10 then acc + lastDigit x + dropLastDigit x else acc + x) 0 xs

-- Exercise 5
-- Indicate whether an INteger could be a valid credit card number
validate :: Integer -> Bool
validate x = lastDigit (sumDigits (doubleEveryOther (toDigits x))) == 0

-- Exercise 6
-- Return a list of moves to be performed to move the stack of discs
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c | x <= 0    = []
              | x == 1    = [(a, b)]
              | otherwise = hanoi (x - 1) a c b ++ [(a, b)] ++ hanoi (x - 1) c b a
