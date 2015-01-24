{- Pong

   Define various constants needed to run the Pong game.
-}

module Pong.Constants where

import Graphics.Gloss

-- | The background color
background :: Color
background = white

-- | A location within a field is an x-coord and a y-coord
type Location = (Int, Int)

-- | The origin point of the ball, at the center
origin :: Location
origin = (0, 0)

-- | The base size on which the ball and paddle sizes are based
baseSize :: Int
baseSize = 20

-- | The total size of the window, in pixels
windowWidth, windowHeight :: Int
windowWidth  = 640
windowHeight = 480

-- | The starting point of the paddle on the left
leftMiddle :: Location
leftMiddle = (0, windowHeight `div` 2)

-- | The starting point of the paddle on the right
rightMiddle :: Location
rightMiddle = (windowWidth-baseSize, windowHeight `div` 2)

-- | The total size of the window, in pixels, stored as @Float@s
windowWidthF, windowHeightF :: Float
windowWidthF = fromIntegral windowWidth
windowHeightF = fromIntegral windowHeight

-- | The starting point of the window
windowX, windowY :: Int
windowX = 200
windowY = 200

-- | How many ticks per second
beatFrequency :: Int -- in beats per second
beatFrequency = 20
