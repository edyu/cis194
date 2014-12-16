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

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4
listToStream:: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)
listToStream [] = error "empty"

instance Show a => Show (Stream a) where
    show s = "[" ++ (showPrefix 20 s)
      where showPrefix :: Show a => Integer -> Stream a -> String
            showPrefix 0 _ = "...]"
            showPrefix n (Cons x xs) = show x ++ "," ++ (showPrefix (n-1) xs)

-- Exercise 5
-- 5a
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- 5b
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

-- 5c
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))
