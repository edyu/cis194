{- Pong

   Defines Ball
-}

module Pong.Ball where

import Pong.Constants

import Graphics.Gloss

import Data.Monoid

-- | The ball
data Ball = Ball { ball_loc   :: Location
                 , ball_color :: Color
                 , ball_size  :: Float
                 }

-- | Get the location of the ball
ballLoc :: Ball -> Location
ballLoc ball = ball_loc ball

-- | Get the color of the ball
ballColor :: Ball -> Color
ballColor ball = ball_color ball

-- | Get the radius of the ball
ballSize :: Ball -> Float
ballSize ball = ball_size ball

-- | Move the ball
moveBall :: (Location -> Location) -> Ball -> Ball
moveBall loc_f b@(Ball { ball_loc = loc }) = b { ball_loc = loc_f loc }

-- | Render the ball at the given location.
renderBall :: Ball -> Picture
renderBall ball =
    color (ballColor ball) (thickCircle thickness radius)
  where
    radius = ballSize ball
    thickness = radius / 2
