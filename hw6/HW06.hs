{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW06 where

import Data.Aeson
import Data.List (sort)
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

-- Exercise 1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object o)   = Object (fmap ynToBool o)
ynToBool (Array  a)   = Array  (fmap ynToBool a)
ynToBool v            = v

-- Exercise 2
parseData :: B.ByteString -> Either String Value
parseData bs = fmap ynToBool $ eitherDecode bs

-- Exercise 3
data Market = Market { marketname :: T.Text
                     , x          :: Double
                     , y          :: Double
                     , state      :: T.Text }
  deriving (Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = case fmap fromJSON $ parseData bs :: Either String (Result [Market]) of
                  Left  e -> Left  e
                  Right r -> case r of
                      (Success m) -> Right m
                      (Error err) -> Left err

-- Exercise 4
loadData :: IO [Market]
loadData = do
  bs <- B.readFile "markets.json"
  case parseMarkets bs of
      Left  e -> fail e
      Right m -> return m

-- Exercise 5
data OrdList a = OrdList { getOrdList :: [a] }
  deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty  = OrdList { getOrdList = [] }
  mappend xs ys = OrdList { getOrdList = sort $ (getOrdList xs) ++ (getOrdList ys) }

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds

-- Exercise 6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search _ _ [] = mempty
search to_m t (m:ms) | t `T.isInfixOf` (marketname m) = to_m m <> search to_m t ms
                     | otherwise = search to_m t ms

-- Exercise 7
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 getFirst (search toFirst)
  where toFirst m = First (Just m)

-- Exercise 8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 getLast (search toLast)
  where toLast m = Last (Just m)
