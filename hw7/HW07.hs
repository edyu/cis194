{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW07 where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
--fibs1 = [ fib i | i <- [0..] ]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = [0,1] ++ (map (\(x,y) -> x + y) $ zip fibs2 (tail fibs2))
