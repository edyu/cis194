{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW09 where

import Test.QuickCheck

import Control.Monad (replicateM)
import System.Random
import Test.HUnit
--import Test.QuickCheck

import Ring
import BST

-- Exercise 1
instance Arbitrary Mod5 where
    arbitrary = genMod5

genMod5 :: Gen Mod5
genMod5 = do
    n <- arbitrary
    return $ mkMod n

instance Arbitrary Mat2x2 where
    arbitrary = genMat2x2
    shrink = shrinkMat2x2

genMat2x2 :: Gen Mat2x2
genMat2x2 = do
    [a, b, c, d] <- replicateM 4 arbitrary
    return $ MkMat a b c d

-- Exercise 2
shrinkMat2x2 :: Mat2x2 -> [Mat2x2]
shrinkMat2x2 (MkMat a b c d) = [MkMat x y z w | x <- shrinkIntegral a
                                              , y <- shrinkIntegral b
                                              , z <- shrinkIntegral c
                                              , w <- shrinkIntegral d]

-- Exercise 3
prop_1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_1 a b c = add (add a b) c == add a (add b c)

prop_2 :: (Ring a, Eq a) => a -> Bool
prop_2 a = add a addId == a && add addId a == a

prop_3 :: (Ring a, Eq a) => a -> Bool
prop_3 a = add a (addInv a) == add (addInv a) a

prop_4 :: (Ring a, Eq a) => a -> Bool
prop_4 a = add a (addInv a) == addId && add (addInv a) a == addId

prop_5 :: (Ring a, Eq a) => a -> a -> Bool
prop_5 a b = add a b == add b a

prop_6 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_6 a b c = mul (mul a b) c == mul a (mul b c)

prop_7 :: (Ring a, Eq a) => a -> Bool
prop_7 a = mul a mulId == a && mul mulId a == a

prop_8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_8 a b c = mul a (add b c) == add (mul a b) (mul a c)

prop_9 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_9 a b c = mul (add b c) a == add (mul b a) (mul c a)

prop_10 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_10 a b c = if a == b && b == c then a == c else a /= b || b /= c

-- Exercise 4
prop_ring :: (Ring a, Eq a) => a -> a -> a -> Property
prop_ring a b c = (conjoin $ [f a b c | f <- [prop_1, prop_6, prop_8, prop_9, prop_10]
                             ]) .&&.
                  (conjoin $ [f x | f <- [prop_2, prop_3, prop_4, prop_7]
                                  , x <- [a, b, c]
                             ]) .&&.
                  prop_5 a b

-- Exercise 5
-- Bool is not a proper ring.
-- Violates prop_4 or #3 of the axioms where
--   For each a in R there exists −a in R such that
--     a + (−a) = (−a) + a = 0 (−a is the additive inverse of a).

-- Exercise 6
isBSTBetween' :: Ord a => Maybe a        -- ^ lower bound, if one exists
              -> Maybe a                 -- ^ upper bound, if one exists
              -> BST a                   -- ^ tree to test
              -> Bool
isBSTBetween' _       _       Leaf = True
isBSTBetween' m_lower m_upper (Node left x right)
    = isBSTBetween' m_lower  (Just x) left  &&
      isBSTBetween' (Just x) m_upper  right &&
      case m_lower of
          Just lower -> lower <= x
          Nothing    -> True
      &&
      case m_upper of
          Just upper -> x <= upper
          Nothing    -> True

-- | Is this a valid BST?
isBST' :: Ord a => BST a -> Bool
isBST' = isBSTBetween' Nothing Nothing

-- Exercise 7
genBST :: (Arbitrary a, Random a, Ord a) => a -> a -> Gen (BST a)
genBST lb ub = do
    make_leaf <- arbitrary
    if make_leaf
        then return Leaf
        else do
            x <- choose (lb, ub)
            lt <- if lb == x
                      then return Leaf
                      else genBST lb x
            ut <- if ub == x
                      then return Leaf
                      else genBST x ub
            return $ Node lt x ut

instance (Arbitrary a, Random a, Ord a) => Arbitrary (BST a) where
    arbitrary = do
        lb <- arbitrary
        ub <- suchThat arbitrary (lb <)
        genBST' lb ub

-- Exercise 8
genBST' :: (Arbitrary a, Random a, Ord a) => a -> a -> Gen (BST a)
genBST' lb ub = sized $ \size -> do
    frequency [ (1, return Leaf)
              , (size, do x <- choose (lb, ub)
                          lt <- if lb == x
                                    then return Leaf
                                    else genBST' lb x
                          ut <- if ub == x
                                    then return Leaf
                                    else genBST' x ub
                          return $ Node lt x ut)
              ]

-- Exercise 9
parserTests :: Test
parserTests = TestList [ "one digit integer" ~:
                         parseAll "3" ~?= Just (3 :: Integer)
                       , "multiple digits integer" ~:
                         parseAll "45" ~?= Just (45 :: Integer)
                       , "negative integer" ~:
                         parseAll "-1" ~?= Just (-1 :: Integer)
                       , "mod5 number" ~:
                         parseAll "1" ~?= Just (MkMod 1)
                       , "mod5 wrap" ~:
                         parseAll "5" ~?= Just (MkMod 0)
                       , "boolean true" ~:
                         parseAll "True" ~?= Just True
                       , "boolean false" ~:
                         parseAll "False" ~?= Just False
                       , "bad boolean" ~:
                         parseAll "false" ~?= (Nothing :: Maybe Bool)
                       , "mat2x2 all zeros" ~:
                         parseAll "[[0,0][0,0]]" ~?= Just (MkMat 0 0 0 0)
                       , "mat2x2 all integers" ~:
                         parseAll "[[1,-2][3,-4]]" ~?= Just (MkMat 1 (-2) 3 (-4))
                       ]
