{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW08 where

import Data.List  (sortBy, stripPrefix)
import Data.Maybe (isJust)
import Data.Monoid
import Text.Read  (readMaybe)
import Control.Monad.Random

-- Exercise 1
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
          -- go evaluates to 'Just ""' on success, or Nothing otherwise
        go xs = parse xs >>= done
        parse []     = Nothing
        parse (x:xs) = do
            n <- readMaybe [x] :: Maybe Int
            strip n xs
        strip n xs = stripPrefix (take n $ repeat 'a') xs
        done [] = Just ""
        done xs = go xs

-- Exercise 2
specialNumbers :: [Int]
specialNumbers = [ n | n <- [1..100]
                     , n `mod` 5 == 0
                     , n `mod` 7 /= 0
                 ]

-- Exercise 3
type StdRand = Rand StdGen

type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
  deriving Show

type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

-- Exercise 4
instance Monoid ArmyCounts where
    mempty  = ArmyCounts { attackers = 0, defenders = 0 }
    mappend x y = ArmyCounts { attackers = (attackers x) - (attackers y),
                               defenders = (defenders x) - (defenders y) }

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults as ds = initial <> (foldr fight initial dies)
  where sortR = sortBy (flip compare)
        dies = zip (sortR as) (sortR ds)
        initial = ArmyCounts { attackers = (length as),
                               defenders = (length ds) }
        fight (a, d) ac | a <= d    = ac <> ArmyCounts { attackers = -1,
                                                         defenders = 0 }
                        | otherwise = ac <> ArmyCounts { attackers = 0,
                                                         defenders = -1 }
