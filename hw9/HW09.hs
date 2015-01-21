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

genMat2x2 :: Gen Mat2x2
genMat2x2 = do
    [x, y, z, w] <- replicateM 4 arbitrary
    return $ MkMat x y z w
