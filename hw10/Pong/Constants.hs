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

-- | The base size on which the ball and paddle sizes are based
baseSize :: Int
baseSize = 20

-- | The amount each key press moves the paddle
moveStep :: Int
moveStep = 10

-- | The total size of the window, in pixels
windowWidth, windowHeight :: Int
windowWidth  = 640
windowHeight = 480

middleX = windowWidth `div` 2
middleY = windowHeight `div` 2

-- | The origin point of the ball, at the center
origin :: Location
origin = (middleX, middleY)

-- | The starting point of the paddle on the left
leftMiddle :: Location
leftMiddle = (0, middleY)

-- | The starting point of the paddle on the right
rightMiddle :: Location
rightMiddle = (windowWidth-baseSize, middleY)

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
