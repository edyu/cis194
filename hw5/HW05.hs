{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW05 where

import Data.Char  (isSpace)
import Data.Maybe (listToMaybe, fromJust)
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

instance Parsable Mat2x2 where
    parse s  = let stripPrefix ('[':'[':xs) = Just xs
                   stripPrefix _            = Nothing
               in  case (stripPrefix $ filter (not . isSpace) s) of
                   Nothing -> Nothing
                   Just xs -> parseInside xs
               where parseInside cs = case reads cs :: [(Integer,String)] of
                         [(x1, (',' : s1))] -> case reads s1 :: [(Integer,String)] of
                             [(y1, (']':'[':s2))] -> case reads s2 :: [(Integer,String)] of
                                 [(x2, (',':s3))] -> case reads s3 :: [(Integer,String)] of
                                      [(y2, (']':']':xs))] -> Just (MkMat (x1, y1) (x2, y2), xs)
                                      _ -> Nothing
                                 _ -> Nothing
                             _ -> Nothing
                         _ -> Nothing

mat2x2ParsingWorks :: Bool
mat2x2ParsingWorks = (parse "[[1,2][3,4]]" == Just ((MkMat (1,2) (3, 4)), "")) &&
                     (parse "[[5,6][7,8]]" == Just ((MkMat (5,6) (7, 8)), "")) &&
                     (parse " [ [ 9 , 3 ] [ 6 , 0 ] ] " == Just ((MkMat (9,3) (6, 0)), "")) &&
                     (parseRing "[[1,2][3,4]] + [[5,6][7,8]]" == Just (MkMat (6,8) (10,12))) &&
                     (parseRing "[[1,2][3,4]] + [[5,6][7,8]] * [[1,0][0,1]] + [[0,0][0,0]]" == Just (MkMat (6,8) (10,12)))

-- Exercise 4
instance Ring Bool where
    addId = False
    addInv = (not)
    mulId = True

    add = (/=)
    mul = (&&)

boolRingWorks :: Bool
boolRingWorks = add True False == True &&
                add False True == True &&
                add True True == False &&
                add False False == False &&
                add True addId == True &&
                add False addId == False &&
                mul True False == False &&
                mul False True == False &&
                mul True True == True &&
                mul False False == False &&
                mul True mulId == True &&
                mul False mulId == False

instance Parsable Bool where
    parse = listToMaybe . reads

boolParsingWorks :: Bool
boolParsingWorks = (parse "True" == Just (True, "")) &&
                   (parse "False" == Just (False, "")) &&
                   (parseRing "True + False * True" == Just True) &&
                   (parseRing "True + True * True" == Just False) &&
                   (parseRing "False + True * False" == Just False) &&
                   (parseRing "False * False + False" == Just False) &&
                   (addId == False) &&
                   (mulId == True)

-- Exercise 5
distribute :: RingExpr a -> RingExpr a
distribute (Lit a) = Lit a
distribute AddId = AddId
distribute MulId = MulId
distribute (AddInv x) = AddInv (distribute x)
distribute (Add x y) = Add (distribute x) (distribute y)
distribute (Mul x (Add y z)) = distribute (Add (Mul x y) (Mul x z))
distribute (Mul (Add x y) z) = distribute (Add (Mul x z) (Mul y z))
distribute (Mul x y) = Mul (distribute x) (distribute y)

distributeWorks :: Bool
distributeWorks = ((eval $ fromJust (parseRing "3 * (4 + 9)" :: Maybe (RingExpr Integer))) == (eval $ distribute $ fromJust (parseRing "3 * (4 + 9)" :: Maybe (RingExpr Integer)))) &&
                  (fromJust (parseRing "3 * (4 + 9)") == (eval $ distribute $ fromJust (parseRing "3 * (4 + 9)" :: Maybe (RingExpr Integer)))) &&
                  ((eval $ fromJust (parseRing "(3 + 4) * 9" :: Maybe (RingExpr Integer))) == (eval $ distribute $ fromJust (parseRing "(3 + 4) * 9" :: Maybe (RingExpr Integer)))) &&
                  (fromJust (parseRing "(3 + 4) * 9") == (eval $ distribute $ fromJust (parseRing "(3 + 4) * 9" :: Maybe (RingExpr Integer)))) &&
                  ((distribute $ fromJust (parseRing "(2 + 3) * (5 + 4)" :: Maybe (RingExpr Integer))) == Add (Add (Mul (Lit 2) (Lit 5)) (Mul (Lit 3) (Lit 5))) (Add (Mul (Lit 2) (Lit 4)) (Mul (Lit 3) (Lit 4))))
