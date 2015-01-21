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
