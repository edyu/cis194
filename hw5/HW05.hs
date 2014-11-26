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

-- Exercise 2
data Mod5 = MkMod Integer
  deriving (Show, Eq)

instance Ring Mod5 where
    addId = MkMod 0
    addInv (MkMod x) = MkMod (5 - x)
    mulId = MkMod 1

    add (MkMod x) (MkMod y) = MkMod (mod (x + y) 5)
    mul (MkMod x) (MkMod y) = MkMod (mod (x * y) 5)

mod5RingWorks :: Bool
mod5RingWorks = add (MkMod 3) (MkMod 4) == MkMod 2 &&
                add (MkMod 1) (MkMod 4) == MkMod 0 &&
                add (MkMod 3) (addInv (MkMod 3)) == MkMod 0 &&
                add (MkMod 4) addId == MkMod 4 &&
                mul (MkMod 3) mulId == MkMod 3 &&
                mul mulId (MkMod 2) == MkMod 2 &&
                mul (MkMod 3) (MkMod 2) == MkMod 1
