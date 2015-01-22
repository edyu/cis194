{-# OPTIONS_GHC -Wall -fno-ghci-sandbox #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module HW10 where

import Graphics.Gloss
import Data.Monoid

main =  display (InWindow "Hello, world!" (200, 200) (200, 200))
                white
                (circle 50 <>
                 (translate (-20) 10    $ circle 10) <>
                 (translate 20    10    $ circle 10) <>
                 (translate 0     (-15) $ scale 1 0.7 $ arc 180 360 20))
