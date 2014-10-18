{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW04 where

import Data.Char (isUpper, isSpace)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (isJust, fromJust)

import BST

-- Exercise 1
ex1 :: a -> b -> b
ex1 _ y = y
-- only implementation

-- Exercise 2
ex2 :: a -> a -> a
ex2 x _ = x
-- only implementation

-- Exercise 3
ex3 :: Int -> a -> a
ex3 _ x = x
-- only implementation

-- Exercise 4
ex4 :: Bool -> a -> a -> a
ex4 True x _ = x
ex4 False _ x = x
-- the other one is to flip the one returned
ex4' :: Bool -> a -> a -> a
ex4' True _ x = x
ex4' False x _ = x

-- Exercise 5
ex5 :: Bool -> Bool
ex5 True = False
ex5 False = True
-- the other one is just return the bool passed in
ex5' :: Bool -> Bool
ex5' x = x

-- Exercise 6
ex6 :: (a -> a) -> a
ex6 _ = error "impossible"
-- given a function, we can't get a "default" value for that type

-- Exercise 7
ex7 :: (a -> a) -> a -> a
ex7 f x = f x
-- the other one just ignore the function passed in
ex7' :: (a -> a) -> a -> a
ex7' _ x = x

-- Exercise 8
ex8 :: [a] -> [a]
ex8 = id
-- tail also works for this
-- infinite implementations

-- Exercise 9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map
-- infinite implementations
-- if input is [] the result must be []

-- Exercise 10
ex10 :: Maybe a -> a
ex10 (Just x) = x
ex10 Nothing = error "impossible"
-- there is no way to get a typed value for Nothing
-- unless we know the type of a beforehand

-- Exercise 11
ex11 :: a -> Maybe a
-- ex11 Nothing = Nothing
ex11 x = Just x
-- you can also always return Nothing
ex11' :: a -> Maybe a
ex11' _ = Nothing

-- Exercise 12
ex12 :: Maybe a -> Maybe a
ex12 = id
-- the only other implementation is just return Nothing
ex12' :: Maybe a -> Maybe a
ex12' _ = Nothing

-- Exercise 13
-- Insert into a binary search tree
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node a y b)
    | f x y == EQ || f x y == LT = Node (insertBST f x a) y b
    | f x y == GT                = Node a y (insertBST f x b)
insertBST _ _ _ = error "impossible"

-- Exercise 14
-- Check whether a list of strings contain only capitalized words
allCaps :: [String] -> Bool
allCaps = all $ all isUpper

-- Exercise 15
-- Drop the trailing whitespace from a string
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- Exercise 16
-- Get the first letter of a list of strings
firstLetters :: [String] -> [Char]
firstLetters xs = map fromJust $ filter isJust $ map safeHead xs

-- Exercise 17
-- Render a proper bracketed list given a list of strings
asList :: [String] -> String
asList xs = '[' : (intercalate "," xs) ++ "]"
