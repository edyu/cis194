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
  deriving (Show, Eq)

type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

-- Exercise 4
instance Monoid ArmyCounts where
    mempty      = ArmyCounts { attackers = 0, defenders = 0 }
    mappend x y = ArmyCounts { attackers = (attackers x) + (attackers y),
                               defenders = (defenders x) + (defenders y) }

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults as ds = foldr fight mempty dies
  where sortR = sortBy (flip compare)
        dies = zip (sortR as) (sortR ds)
        fight (a, d) ac
            | a <= d    = ac <> ArmyCounts { attackers = -1,
                                             defenders = 0 } 
            | otherwise = ac <> ArmyCounts { attackers = 0,
                                             defenders = -1 }

battleResultsWorks :: Bool
battleResultsWorks = battleResults [3,6,4] [5,5] == ArmyCounts { attackers = -1,
                                                                 defenders = -1 } &&
                     battleResults [3,6,4] [5,6] == ArmyCounts { attackers = -2,
                                                                 defenders = 0 } &&
                     battleResults [4] [3,2]     == ArmyCounts { attackers = 0,
                                                                 defenders = -1 }

-- Exercise 5
battle :: ArmyCounts -> StdRand ArmyCounts
battle ac = do ad <- dice attacks
               dd <- dice defends
               let br = battleResults ad dd
               return ArmyCounts { attackers = (attackers ac) + (attackers br),
                                   defenders = (defenders ac) + (defenders br) }
  where attacks = case attackers ac of
                       n | n > 3 -> 3
                       n | n > 0 -> n - 1
                       _         -> 0
        defends = case defenders ac of
                       n | n >= 2 -> 2
                       n | n == 1 -> 1
                       _          -> 0
        dice n = sequence (replicate n dieRoll)

-- Exercise 6
invade :: ArmyCounts -> StdRand ArmyCounts
invade ac = do bc <- battle ac
               case bc of
                    ArmyCounts { attackers = x } | x < 2  -> return bc
                    ArmyCounts { defenders = x } | x <= 0 -> return bc
                    _                                     -> invade bc

-- Exercise 7
(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

successProb:: ArmyCounts -> StdRand Double
successProb ac = tryInvade count 0
  where count = 1000
        tryInvade n p
            | n == 0    = return (p // count)
            | otherwise = do let m = n - 1
                             b <- invade ac
                             case b of
                                  ArmyCounts { defenders = x }
                                      | x <= 0    -> tryInvade m (p + 1)
                                      | otherwise -> tryInvade m p
