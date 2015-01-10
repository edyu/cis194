{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW07 where

import System.Random

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
--    show s = show (take 20 (streamToList s))

-- Exercise 5
-- 5a
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- 5b
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- 5c
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 6
-- 6a
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- 6b
{- ruler number
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...]
[0,1,0,2,0,1,0,3,0,1, 0, 2, 0, 1, 0, 4,...]
-}
ruler :: Stream Integer
ruler = streamMap rulerInt (streamMap (+1) nats)
  where rulerInt n | n `mod` 2 /= 0 = 0
        rulerInt n = 1 + (rulerInt (n `div` 2))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler1 :: Stream Integer
ruler1 = myruler [0..]
  where myruler (x:xs) = interleaveStreams (streamRepeat x) (myruler xs)
        myruler x      = listToStream x

-- Exercise 7
randomList :: (Random a, RandomGen g) => g -> [a]
randomList g = let (a, g') = random g in
               a : randomList g'

-- Exercise 8
randomInts  :: Int -> [Int]
randomInts n = let g = mkStdGen 37 in
               take n $ randomList g

-- Exercise 9
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)

{-
main :: IO ()
main = print $ show $ minMax $ randomInts 1000000
-}
-- 292 MB total memory in use

-- Exercise 10
minMax1 :: [Int] -> Maybe (Int, Int)
minMax1 []     = Nothing
minMax1 (x:xs) = Just (getMinMax x x xs)
  where getMinMax u v []     = (u, v)
        getMinMax u v (y:[]) =  (min u y, max v y)
        getMinMax u v (y:ys) = u `seq` v `seq` getMinMax (min u y) (max v y) ys

main :: IO ()
main = print $ show $ minMax1 $ randomInts 1000000
-- 1 MB total memory in use

-- Exercise 11
data Matrix = Matrix (Integer, Integer) (Integer, Integer)
  deriving (Show, Eq)

mmap :: (Integer -> Integer) -> Matrix -> Matrix
mmap f (Matrix (x1, y1) (x2, y2)) = Matrix (f x1, f y1) (f x2, f y2)

instance Num Matrix where
    (*) (Matrix (x1, y1) (x2, y2)) (Matrix (u1, v1) (u2, v2)) = Matrix (x1 * u1 + y1 * u2, x1 * v1 + y1 * v2) (x2 * u1 + y2 * u2, x2 * v1 + y2 * v2)
    (+) (Matrix (x1, y1) (x2, y2)) (Matrix (u1, v1) (u2, v2)) = Matrix (x1 + u1, y1 + v1) (x2 + u2, y2 + v2)
    fromInteger x = Matrix (x, 0) (0, x)
    negate m = mmap negate m
    abs m = mmap abs m
    signum m = mmap signum m

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let f = Matrix (1, 1) (1, 0)
             fn = f ^ n
         in case fn of
                Matrix (_, x) _ -> x
