{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW09 where

import Test.QuickCheck

import Ring

-- Exercise 1
instance Arbitrary Mod5 where
    arbitrary = genMod5

genMod5 :: Gen Mod5
genMod5 = do
    n <- arbitrary
    return $ mkMod n
