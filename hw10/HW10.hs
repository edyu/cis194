{-# OPTIONS_GHC -Wall -fno-ghci-sandbox #-}
{-
Name: Ed Yu
Collaborators:
Notes:
-}

module Main where

import Pong


main :: IO ()
main = playPong

{--
main =  display (InWindow "Pong" (640, 480) (500, 500))
                white
                (circle 50 <>
                 (translate (-20) 10    $ circle 10) <>
                 (translate 20    10    $ circle 10) <>
                 (translate 0     (-15) $ scale 1 0.7 $ arc 180 360 20))
--}
