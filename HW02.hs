{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators: none
Notes:
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

-- Exercise 1
-- Whether a certain word is formable from the tiles in a Scrabble hand
formableBy :: String -> Hand -> Bool
formableBy [] _        = True
formableBy word hand | length word > length hand = False
formableBy (s:ss) hand = if s `elem` hand
                         then formableBy ss (delete s hand)
                         else False

-- Exercise 2
-- Give a list of all valid Scrabble words formable from a certain hand
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Exercise 3
-- Check to see if a given word matches a template
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _  = False
wordFitsTemplate _ _ []  = False
wordFitsTemplate t _ s
  | length t /= length s = False
wordFitsTemplate (t:ts) hand (s:ss)
  | t /= '?' && t /= s   = False
  | t == s               = wordFitsTemplate ts hand ss
  | s `elem` hand        = wordFitsTemplate ts (delete s hand) ss
  | otherwise            = False

-- Exercise 4
-- Produce all valid Scrabble words that match a iven template
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t hand = filter (\w -> wordFitsTemplate t hand w) (wordsFrom (hand ++ (filter (\x -> x /= '?') t)))

-- Exercise 5
-- Give the point value of any word
scrabbleValueWord :: String -> Int
scrabbleValueWord word = sum (map scrabbleValue word)

-- Exercise 6
-- Select out words that have the maximum point value
bestWords :: [String] -> [String]
bestWords ws = let values   = map scrabbleValueWord ws
                   maxValue = maximum values
               in map snd (filter (\(v, _) -> v == maxValue) (zip values ws))

-- Exercise 7
-- Compute the value of playing a given word on a given template
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate tmpl word = mplier * (normal + bonus)
                                  where
                                    normal = scrabbleValueWord word
                                    mfunc '2' acc = 2 * acc
                                    mfunc '3' acc = 3 * acc
                                    mfunc _   acc = acc
                                    mplier = foldr mfunc 1 tmpl
                                    val w = scrabbleValue w
                                    extra ('D',w) = val w
                                    extra ('T',w) = 2 * (val w)
                                    extra (_,_)   = 0
                                    values = (zip tmpl word)
                                    bonus = sum (map extra values)
