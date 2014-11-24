{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW05 where

import Ring
import Parser

-- Exercise 1
-- Example test definition
intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                  (addId == (0 :: Integer))
