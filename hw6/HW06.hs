{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

-- Exercise 1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object x)   = Object (fmap ynToBool x)
ynToBool (Array x)    = Array  (fmap ynToBool x)
ynToBool x            = x

-- Exercise 2
parseData :: B.ByteString -> Either String Value
parseData x = fmap ynToBool $ eitherDecode x
