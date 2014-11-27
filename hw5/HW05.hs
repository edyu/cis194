{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW05 where

import Data.Maybe    ( listToMaybe )
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

instance Parsable Mod5 where
    parse s = case ((listToMaybe . reads) s) of
                Nothing     -> Nothing
                Just (x, y) -> Just (MkMod x, y)

mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "3" == Just (MkMod 3, "")) &&
                   (parseRing "1 + 2 * 5" == Just (MkMod 1)) &&
                   (addId == (MkMod 0))

-- Exercise 3
data Mat2x2 = MkMat (Integer, Integer) (Integer, Integer)
  deriving (Show, Eq)

instance Ring Mat2x2 where
    addId = MkMat (0, 0) (0, 0)
    addInv (MkMat (x1, y1) (x2, y2)) = MkMat (-x1, -y1) (-x2, -y2)
    mulId = MkMat (1, 0) (0, 1)

    add (MkMat (x1, y1) (x2, y2)) (MkMat (u1, v1) (u2, v2)) = MkMat (x1 + u1, y1 + v1) (x2 + u2, y2 + v2)
    mul (MkMat (x1, y1) (x2, y2)) (MkMat (u1, v1) (u2, v2)) = MkMat (x1 * u1 + y1 * u2, x1 * v1 + y1 * v2) (x2 * u1 + y2 * u2, x2 * v1 + y2 * v2)

mat2x2RingWorks :: Bool
mat2x2RingWorks =  let mat1 = MkMat (1, 2) (3, 4)
                       mat2 = MkMat (5, 6) (7, 8)
                   in  add mat1 mat2 == MkMat (6, 8) (10, 12) &&
                       add mat2 mat1 == MkMat (6, 8) (10, 12)  &&
                       add mat1 (addInv mat1) == addId &&
                       add mat1 addId == mat1 &&
                       mul mat1 mulId == mat1 &&
                       mul mulId mat2 == mat2 &&
                       mul mat1 mat2 == MkMat (19, 22) (43, 50)
