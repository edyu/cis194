{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW09 where

import Test.QuickCheck

import Control.Monad (replicateM)
import Ring

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
